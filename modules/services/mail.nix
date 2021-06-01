{ config, pkgs, lib, ... }:
<<<<<<< HEAD
let
  cfg = config.hazel.services.mailserver;
  release = "nixos-20.09";
in 
with lib; {
  # couldn't get `niv` to work here, but whatev
  imports = [
    (builtins.fetchTarball {
      # wew
      url = 
        "https://gitlab.com/simple-nixos-mailserver/nixos-mailserver/-/archive/${release}/nixos-mailserver-${release}.tar.gz";
      sha256 = "0vsvgxxg5cgmzwj98171j7h5l028f1yq784alb3lxgbk8znfk51y";
    })
  ];
=======
{
  mailserver = {
    enable = true;
    fqdn = "mail.knightsofthelambdacalcul.us";
    domains = [ "knightsofthelambdacalcul.us" ];
>>>>>>> refs/remotes/origin/canon

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
