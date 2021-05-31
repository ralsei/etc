{ config, pkgs, lib, ... }:
let
  cfg = config.hazel.services.mailserver;
in 
with lib; {
  options = {
    hazel.services.mailserver = {
      enable = mkOption {
        default = false;
        type = with types; bool;
      };
    };
  };

  config = mkIf cfg.enable {
    mailserver = {
      enable = true;
      fqdn = "mail.knightsofthelambdacalcul.us";
      domains = [ "knightsofthelambdacalcul.us" ];

      loginAccounts = {
        "hazel@knightsofthelambdacalcul.us" = {
          hashedPasswordFile = /etc/snm-hashed-passwd;
          aliases = [ "postmaster@knightsofthelambdacalcul.us" ];
        };
      };

      certificateScheme = 3; # use letsencrypt

      enableImap = true;
      enableImapSsl = true;
      enablePop3 = false;
      enablePop3Ssl = false;

      enableManageSieve = true;
    };
  };
}
