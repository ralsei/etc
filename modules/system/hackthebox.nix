{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.hackTheBox;
in
with lib; {
  options = {
    hazel.hackTheBox = {
      enable = mkOption {
        default = false;
        type = with types; bool;
        description = ''
          Enable the Hack The Box OpenVPN profiles.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    services.openvpn.servers = let
      mkConn = c: {
        config = '' config ${toString c} '';
        autoStart = false;
      };
    in {
      startingPoint = mkConn /etc/htb/hazel-startingpoint.ovpn;
      htb = mkConn /etc/htb/hazel.ovpn;
    };
  };
}
