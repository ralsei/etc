{ config, pkgs, lib, ... }:
{
  mailserver = {
    enable = true;
    fqdn = "mail.knightsofthelambdacalcul.us";
    domains = [ "knightsofthelambdacalcul.us" ];

    loginAccounts = {
      "hazel@knightsofthelambdacalcul.us" = {
        # TODO: fix
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
}
