{ config, pkgs, lib, ... }:
let
  cfg = config.hazel.services.nextcloud;
  nginxCfg = config.hazel.services.nginx;
in
with lib; {
  options = {
    hazel.services.nextcloud = {
      enable = mkOption {
        default = false;
        type = with types; bool;
      };
    };
  };
  
  config = mkIf cfg.enable {
    services.nextcloud = {
      enable = true;
      package = pkgs.nextcloud21;
      hostName = "cloud.bicompact.space";

      # nginx.enable = nginxCfg.enable;
      https = nginxCfg.ssl;
      maxUploadSize = "5G";

      config = {
        dbtype = "pgsql";
        dbuser = "nextcloud";
        dbhost = "/run/postgresql";
        dbname = "nextcloud";
        dbpassFile = toString config.age.secrets.nextcloudDbPass.path;

        adminuser = "hazel";
        adminpassFile = toString config.age.secrets.nextcloudPass.path;
      };
    };

    services.postgresql = {
      enable = true;
      ensureDatabases = [ "nextcloud" ];
      ensureUsers = [
        { name = "nextcloud";
          ensurePermissions."DATABASE nextcloud" = "ALL PRIVILEGES";
        }
      ];
    };

    systemd.services."nextcloud-setup" = {
      requires = [ "postgresql.service" ];
      after = [ "postgresql.service" ];
    };

    services.nginx.virtualHosts."cloud.bicompact.space" = 
      if nginxCfg.enable then {
        forceSSL = nginxCfg.ssl;
        enableACME = nginxCfg.ssl;
      } else {};
  };
}
