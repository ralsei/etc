{ config, pkgs, lib, ... }:
{
  networking.networkmanager = {
    enable = true;
    wifi.backend = "iwd";
  };
  networking.resolvconf.enable = true;

  # opennic: https://www.opennic.org
  networking.nameservers = [ "172.98.193.42" "66.70.228.164"
                             "128.31.0.72" "147.135.113.37" ];

  # wireguard vpn, all but the client IP
  networking.wg-quick.interfaces.wg0 = {
    privateKeyFile = "/etc/wg-privkey"; # go away

    peers = [
      {
        # server's public key
        publicKey = "JvRPh0i1JhSh+yBnj5gef5QExK99FpSzf0QTJg4Usk0=";

        # server IP
        allowedIPs = [ "10.66.66.1/32" "fd42:42:42::1/128"
                       "0.0.0.0/0" "::/0" ];

        # the server
        endpoint = "45.79.140.85:51820";

        # keep the NAT alive
        persistentKeepalive = 25;
      }
    ];
  };
}
