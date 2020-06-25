{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.wireguard;
in
with lib; {
  options = {
    hazel.wireguard = {
      enable = mkOption {
        default = false;
        type = with types; bool;
        description = ''
          Enable my personal WireGuard VPN.
        '';
      };

      addresses = mkOption {
        type = with types; listOf str;
        description = ''
          The IPv4 and IPv6 address of the client.
        '';
      };

      routeAll = mkOption {
        default = false;
        type = with types; bool;
      };
    };
  };

  config = mkIf cfg.enable {
    networking.wg-quick.interfaces.wg0 = {
      privateKeyFile = "/etc/wg-privkey"; # go away
      address = cfg.addresses;

      peers = [{
        # server's public key
        publicKey = "JvRPh0i1JhSh+yBnj5gef5QExK99FpSzf0QTJg4Usk0=";

        # server IP
        allowedIPs = [ "10.66.66.1/32" "fd42:42:42::1/128" ] ++ 
                       (if cfg.routeAll then [ "0.0.0.0/0" "::/0" ]
                        else []);

        # the server
        endpoint = "45.79.140.85:51820";

        # keep the NAT alive
        persistentKeepalive = 25;
      }];
    };
  };
}
