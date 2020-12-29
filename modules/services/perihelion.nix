{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.services.perihelion;
  nginxCfg = config.hazel.services.nginx;
in
with lib; {
  options = {
    hazel.services.perihelion = {
      enable = mkOption {
        default = false;
        type = with types; bool;
      };
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      hazel.perihelion
    ];

    environment.etc."perihelion/users.txt".text = ''
      hazel|https://knightsofthelambdacalcul.us/
      if|https://tilde.club/~if/
      haskal|https://tilde.town/~haskal/
      not-haskal|https://awoo.systems/
      5225225|https://5snb.club/
      cadence|https://cadence.moe/
    '';

    systemd.services.perihelion = {
      enable = true;
      description = "Simple webring manager";

      serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.hazel.perihelion}/bin/perihelion ${pkgs.hazel.perihelion}/static /etc/perihelion/users.txt";
        Restart = "on-failure";
      };

      after = [ "network.target" ];
      wantedBy = [ "network.target" ];
    };

    services.nginx.virtualHosts."ring.knightsofthelambdacalcul.us" =
      if nginxCfg.enable then {
        forceSSL = nginxCfg.ssl;
        enableACME = nginxCfg.ssl;

        locations."/" = {
          proxyPass = "http://localhost:5020";
        };

        extraConfig = ''
          add_header 'Access-Control-Allow-Origin' '*';
        '';
      } else {};
  };
}
