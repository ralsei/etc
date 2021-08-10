{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.services.nginx;
  policyFile = pkgs.writeText "wks-policy" "";
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

      wkd = mkOption {
        type = types.submodule {
          options = {
            enable = mkEnableOption "web key directory";
            directory = mkOption {
              type = lib.types.path;
              description = ''
                Which directory to use for Web Key Directory.
              '';
            };
          };
        };
      };
    };
  };

  config = mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = [ 80 443 ];

    # environment.systemPackages = with pkgs; [ hazel.ziodyne-blog ];

    security.acme = {
      email = "hazel@bicompact.space";
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

      upstreams = {
        "ws-backend" = {
          servers = { "127.0.0.1:8989" = {}; };
          extraConfig = ''
            ip_hash;
          '';
        };
      };

      virtualHosts = let
        mkVHost = serverAliases: root: locations: {
          inherit locations root serverAliases;

          forceSSL = cfg.ssl;
          enableACME = cfg.ssl;
        };

        mkRedirect = subdomain: {
          forceSSL = cfg.ssl;
          enableACME = cfg.ssl;
          locations."/" = 
          let prefix = if cfg.ssl then "https" else "http";
          in
          {
            return = "301 ${prefix}://${subdomain}.bicompact.space$request_uri";
          };
        };
      in {
        "bicompact.space" = 
          (mkVHost [ "www.bicompact.space" ] "/var/www/html"
            (let
              subText = "IF_YOURE_READING_THIS_EMAIL_ME_I_MESSED_UP_THIS_IS_NOT_A_JOKE";
            in {
              "/" = {
               extraConfig = ''
                 ssi on;
                 sub_filter_once off;
                 sub_filter_types *;
                 sub_filter ${subText} $rnd_text;
               '';
               tryFiles = "$uri $uri/ =404";
             };

             "/index.html" = {
               extraConfig = ''
                 ssi on;
                 sub_filter_once off;
                 sub_filter_types *;
                 sub_filter ${subText} $rnd_text;
               '';
             };

             "=/.well-known/openpgpkey/policy" = mkIf cfg.wkd.enable {
               alias = policyFile;
               extraConfig = ''
                 add_header 'Access-Control-Allow-Origin' '*';
               '';
             };
             "/.well-known/openpgpkey/" = mkIf cfg.wkd.enable {
               alias = "${cfg.wkd.directory + "/bicompact.space"}/";
               extraConfig = ''
                 add_header 'Access-Control-Allow-Origin' '*';
               '';
             };
           }));
        "blog.bicompact.space" = {
          forceSSL = cfg.ssl;
          enableACME = cfg.ssl;
          locations."/" = 
          let prefix = if cfg.ssl then "https" else "http";
          in
          {
            return = "301 ${prefix}://bicompact.space$request_uri";
          };
        };
        "mail.bicompact.space" =
          (mkVHost [] "/var/www/notawebsite" {});
        # docker run --detach -p 8989:8989 
        #   -e PORT=8989 
        #   -e JWT_SECRET_KEY=lolyouthought
        #   -e STORAGE_URL=https://remarkable.bicompact.space
        #   -v rmfakecloud-data:/data --restart=unless-stopped
        #   ddvk/rmfakecloud
        "remarkable.bicompact.space" = {
          forceSSL = cfg.ssl;
          enableACME = cfg.ssl;
          locations."/" = {
            extraConfig = ''
              proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
              proxy_set_header Host $host;

              proxy_pass http://localhost:8989;

              proxy_http_version 1.1;
              proxy_set_header Upgrade $http_upgrade;
              proxy_set_header Connection "upgrade";
            '';
          };
        };

        "knightsofthelambdacalcul.us" = {
          forceSSL = cfg.ssl;
          enableACME = cfg.ssl;
          locations."/" = {
            return = "301 http://bicompact.space$request_uri";
          };
          serverAliases = [ "www.knightsofthelambdacalcul.us" ];
        };
        "blog.knightsofthelambdacalcul.us" = (mkRedirect "blog");
        "cloud.knightsofthelambdacalcul.us" = (mkRedirect "cloud");
        "git.knightsofthelambdacalcul.us" = (mkRedirect "git");
        "ring.knightsofthelambdacalcul.us" = (mkRedirect "ring");
        "vault.knightsofthelambdacalcul.us" = (mkRedirect "vault");
      };
    };
  };
}
