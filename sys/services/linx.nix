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
      bind = 0.0.0.0:6730

      sitename = "qtp2paste"
      siteurl = "http://p.knightsofthelambdacalcul.us"

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

    services.nginx.virtualHosts."p.knightsofthelambdacalcul.us" =
      if nginxCfg.enable then {
        forceSSL = nginxCfg.ssl;
        enableACME = nginxCfg.ssl;
        serverAliases = [ "p.qtp2t.club" ];

        locations."/" = {
          extraConfig = ''
            fastcgi_param  SCRIPT_FILENAME    $document_root$fastcgi_script_name;
            fastcgi_param  QUERY_STRING       $query_string;
            fastcgi_param  REQUEST_METHOD     $request_method;
            fastcgi_param  CONTENT_TYPE       $content_type;
            fastcgi_param  CONTENT_LENGTH     $content_length;
            fastcgi_param  SCRIPT_NAME        $fastcgi_script_name;
            fastcgi_param  REQUEST_URI        $request_uri;
            fastcgi_param  DOCUMENT_URI       $document_uri;
            fastcgi_param  DOCUMENT_ROOT      $document_root;
            fastcgi_param  SERVER_PROTOCOL    $server_protocol;
            fastcgi_param  GATEWAY_INTERFACE  CGI/1.1;
            fastcgi_param  SERVER_SOFTWARE    nginx/$nginx_version;
            fastcgi_param  REMOTE_ADDR        $remote_addr;
            fastcgi_param  REMOTE_PORT        $remote_port;
            fastcgi_param  SERVER_ADDR        $server_addr;
            fastcgi_param  SERVER_PORT        $server_port;
            fastcgi_param  SERVER_NAME        $server_name;
            
            fastcgi_index  index.php;
            
            fastcgi_param  REDIRECT_STATUS    200;

            fastcgi_pass 127.0.0.1:6730;
          '';
        };
        extraConfig = ''
          client_max_body_size 4096M;
        '';
      } else {};
  };
}
