{ inputs, config, lib, pkgs, ... }:
let
  cfg = config.hazel.services.lemniscation;
  nginxCfg = config.hazel.services.nginx;
in
with lib; {
  options.hazel.services.lemniscation.enable = mkEnableOption "lemniscation";

  config = mkIf cfg.enable {
    services.nginx.virtualHosts = if nginxCfg.enable then 
    let
      redirect = {
        forceSSL = nginxCfg.ssl;
        enableACME = nginxCfg.ssl;
        locations."/" = {
          return = "301 http://lemniscation.com";
        };
      };
    in {
      "lemniscation.com" = {
        forceSSL = nginxCfg.ssl;
        enableACME = nginxCfg.ssl;

        serverAliases = [ "www.lemniscation.com" ];
        root = "/var/www/lemniscation";

        extraConfig = "disable_symlinks off;";
      }; 
      "lemniscation.knightsofthelambdacalcul.us" = redirect; 
    } else {};
  };
}
