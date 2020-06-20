{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.services.bitwarden;
  nginxCfg = config.hazel.services.nginx;
in
with lib; {
  options = {
    hazel.services.bitwarden = {
      enable = mkOption {
        default = false;
        type = with types; bool;
      };
    };
  };

  config = mkIf cfg.enable {
    # not forwarded -- testing only due to HTTPS restrictions
    networking.firewall.allowedTCPPorts = [ 8080 ];

    services.bitwarden_rs = {
      enable = true;
      config = {
        domain = "https://vault.qtp2t.club";
        signupsAllowed = false;
        rocketPort = 8080;
        rocketLog = "critical";
      };
      dbBackend = "sqlite";
    };

    services.nginx.virtualHosts."vault.qtp2t.club" =
      if nginxCfg.enable then {
        forceSSL = nginxCfg.ssl;
        enableACME = nginxCfg.ssl;

        locations = {
          "/" = {
            proxyPass = "http://localhost:8080";
          };

          "/notifications/hub" = {
            proxyPass = "http://localhost:3012";
            extraConfig = ''
              proxy_set_header Upgrade $http_upgrade;
              proxy_set_header Connection "upgrade";
            '';
          };

          "/notifications/hub/negotiate" = {
            proxyPass = "http://localhost:8080";
          };
        };

        extraConfig = ''
          client_max_body_size 128M;
        '';
      } else {};
  };
}
