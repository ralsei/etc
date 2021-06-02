{ config, pkgs, lib, ... }:
let
  cfg = config.hazel.services.gitea;
  nginxCfg = config.hazel.services.nginx;
in
with lib; {
  options = {
    hazel.services.gitea = {
      enable = mkOption {
        default = false;
        type = with types; bool;
      };
    };
  };
  
  config = mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = [ 2222 3000 ]; # 3000 isn't forwarded

    services.gitea = {
      enable = true;
      disableRegistration = true; # GO AWAY (or email me)

      repositoryRoot = "/var/lib/gitea/git"; # hold-over from alpine

      appName = "very good git repositories";
      domain = "git.knightsofthelambdacalcul.us";
      rootUrl = "https://git.knightsofthelambdacalcul.us";

      database = {
        type = "sqlite3";
        createDatabase = false; # hold-over from alpine
        user = "hazel";
        passwordFile = toString config.age.secrets.giteaDbPass.path;
      };
      ssh = {
        enable = true;
        clonePort = 2222;
      };

      settings = {
        server = {
          SSH_DOMAIN = "knightsofthelambdacalcul.us";
          START_SSH_SERVER = true;
        };
        openid = {
          ENABLE_OPENID_SIGNIN = false;
          ENABLE_OPENID_SIGNUP = false;
        };
        ui = {
          DEFAULT_THEME = "arc-green";
        };
        security = {
          INSTALL_LOCK = true;
          PASSWORD_COMPLEXITY = "off";
        };
      };
    };

    services.nginx.virtualHosts."git.knightsofthelambdacalcul.us" =
      if nginxCfg.enable then {
        enableACME = nginxCfg.ssl;
        forceSSL = nginxCfg.ssl;

        locations."/" = {
          proxyPass = "http://127.0.0.1:3000";
          proxyWebsockets = true;
          extraConfig = ''
            proxy_ssl_server_name on;
            proxy_pass_header Authorization;
          '';
        };
      } else {};
  };
}
