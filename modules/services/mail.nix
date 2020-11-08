{ config, pkgs, lib, ... }:
let
  cfg = config.hazel.services.mailserver;
in 
with lib; {
  # couldn't get `niv` to work here, but whatev
  imports = [
    (builtins.fetchTarball {
      # wew
      url = 
        "https://gitlab.com/simple-nixos-mailserver/nixos-mailserver/-/archive/066dba1b2ffbbe39dab9b1c3c1d759423b7f7b38/nixos-mailserver-066dba1b2ffbbe39dab9b1c3c1d759423b7f7b38.tar.gz";
      sha256 = "1ypzj3rjvvmpms37dcbxfrnqrn281nlsy56rr989gkqfmm53mxd6";
    })
  ];

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
