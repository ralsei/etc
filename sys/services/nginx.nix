{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.services.nginx;
in
with lib; {
  options = {
    hazel.services.nginx = {
      enable = mkOption {
        default = false;
        type = with types; bool;
      };

      ssl = mkOption {
        default = false;
        type = with types; bool;
      };
    };
  };

  config = mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = [ 80 443 ];

    environment.systemPackages = with pkgs; [ hazel.ziodyne-blog ];

    security.acme = {
      email = "me@qtp2t.club";
      acceptTerms = true;
    };

    services.nginx = {
      enable = true;

      recommendedProxySettings = true;
      recommendedTlsSettings = true;
      recommendedGzipSettings = true;
      recommendedOptimisation = true;

      # stupid quote memery
      commonHttpConfig = with builtins; let
        quoteNames = [
          "unexpected T_PAAMAYIM_NEKUDOTAYIM"
          "is currently at soup"
          "no longer FDA approved"
          "a very real and not fake human"
          "two trucks"
          "not Heat O'Brien"
          "∃x∈creatures reading this. x understands this message"
          "javascript was a mistake"
          "you are now breathing manually"
          "brought to you by: Fruit"
        ];
        quoteIDs = map toString (lists.range 0 9);
        zippedQuotes = zipLists quoteIDs quoteNames; # [ { fst: <id>; snd: <name>; } ]

        genQuote = quote: ''~${quote.fst}$ "${quote.snd}";'';
      in ''
        map $msec $rnd_text {
          default "idk";
          ${concatStringsSep "\n" (map genQuote zippedQuotes)}
        }
      '';

      virtualHosts = let
        mkVHost = uri: dir: { "${uri}" = {
          forceSSL = cfg.ssl;
          enableACME = cfg.ssl;
          root = dir;
        }; };
      in {
        "qtp2t.club" = {
          forceSSL = cfg.ssl;
          enableACME = cfg.ssl;
          root = "/var/www/html";

          locations = let
            subText = "IF_YOURE_READING_THIS_EMAIL_ME_I_MESSED_UP_THIS_IS_NOT_A_JOKE";
          in {
            "/" = {
              extraConfig = ''
                ssi on;
                if ($http_user_agent ~* (curl|wget)) {
                  return 301 http://$host/hrl.7;
                }

                sub_filter ${subText} $rnd_text;
              '';
              tryFiles = "$uri $uri/ =404";
            };

            "/index.html" = {
              extraConfig = ''
                ssi on;
                sub_filter ${subText} $rnd_text;
              '';
            };

            "/hrl.7" = {
              extraConfig = ''
                sub_filter_types *;
                sub_filter ${subText} $rnd_text;
              '';
            };
          };
        };
      } // (mkVHost "blog.qtp2t.club" "${pkgs.hazel.ziodyne-blog}")
        // (mkVHost "lemniscation.qtp2t.club" "/var/www/lemniscation");
    };
  };
}
