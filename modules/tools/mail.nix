{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.mail;
in
with lib; {
  options = {
    hazel.mail = {
      enable = mkOption {
        default = false;
        type = with types; bool;
        description = ''
          Enable reading and storing of email.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    hazel.home = {
      accounts.email = {
        maildirBasePath = "var/mail";

        accounts.lambdamail = rec {
          primary = true;
          address = "hazel@knightsofthelambdacalcul.us";
          userName = address;
          realName = "Hazel Levine";
          passwordCommand = "${pkgs.gnupg}/bin/gpg --decrypt /home/hazel/.config/lambdamail-pass.gpg";

          imap = {
            host = "knightsofthelambdacalcul.us";
            tls.enable = false;
          };
          offlineimap.enable = true;

          smtp.host = "knightsofthelambdacalcul.us";
        };

        accounts.protonmail = rec {
          address = "me@qtp2t.club";
          userName = address;
          realName = "Hazel Levine";
          # couldn't figure out how to use pass here.
          # also storing my password in plaintext isn't really an issue when it only
          # works if it's on my machine.
          passwordCommand = "${pkgs.coreutils}/bin/cat /home/hazel/.config/hydroxide-pass";

          imap = {
            host = "127.0.0.1";
            port = 1143;
            tls.enable = false;
          };
          offlineimap.enable = true;

          smtp = {
            host = "127.0.0.1";
            port = 1025;
            tls.useStartTls = true;
          };
        };
      };

      programs.offlineimap.enable = true;

      systemd.user.services.hydroxide = {
        Unit = {
          Description = "Bridge to ProtonMail";
          After = [ "network-online.target" ];
          Wants = [ "network-online.target" ];
        };

        Service = {
          Type = "simple";
          ExecStart = "${pkgs.hydroxide}/bin/hydroxide serve";
        };

        Install = {
          WantedBy = [ "multi-user.target" ];
        };
      };
    };
  };
}
