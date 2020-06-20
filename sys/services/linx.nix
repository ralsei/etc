{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.services.linx;
  nginxCfg = config.hazel.services.nginx;
in
with lib; {
  options = {
    hazel.services.linx = {
      enable = mkOption {
        default = false;
        type = with types; bool;
      };
    };
  };

  config = mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = [ 6730 ]; # not forwarded

    environment.systemPackages = with pkgs; [
      hazel.linx-server
    ];

    environment.etc."linx.ini".text = ''
      bind = 127.0.0.1:6730

      sitename = "qtp2paste"
      siteurl = "http://p.qtp2t.club"

      filespath = /var/www/linx/files/
      metapath = /var/www/linx/meta/

      authfile = /etc/linx.key
    '';

    systemd.services.linx-server = {
      enable = true;
      description = "Linx file hosting server";

      serviceConfig = {
        Type = "simple";
        User = "linx";
        WorkingDirectory = "/var/www/linx";
        ExecStart = "${pkgs.hazel.linx-server}/bin/linx-server -config /etc/linx.ini";
        Restart = "on-failure";
      };

      after = [ "network.target" ];
      wantedBy = [ "network.target" ];
    };

    users.groups = { linx = {}; };
    users.users.linx = {
      extraGroups = [ "linx" ];
    };

    services.nginx.virtualHosts."p.qtp2t.club" =
      if nginxCfg.enable then {
        forceSSL = nginxCfg.ssl;
        enableACME = nginxCfg.ssl;

        locations."/" = {
          proxyPass = "http://127.0.0.1:6730";
        };
        extraConfig = ''
          client_max_body_size 4096M;
        '';
      } else {};
  };
}
