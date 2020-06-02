{ config, pkgs, lib, ... }:
{
  # hostname and hostid (for zfs)
  networking.hostName = "hyacinth";
  networking.hostId = "3ae0d799";
  
  # it works I guess
  networking.networkmanager.enable = true;
  networking.resolvconf.enable = true;

  # opennic: https://www.opennic.org
  networking.nameservers = [ "172.98.193.42" "66.70.228.164"
                             "128.31.0.72" "147.135.113.37" ];

  # disable global DHCP, but enable it on all my interfaces
  networking.useDHCP = false;
  networking.interfaces.enp3s0f0.useDHCP = true;
  networking.interfaces.enp4s0.useDHCP = true;
  networking.interfaces.wlp1s0.useDHCP = true;

  # wireguard vpn
  networking.wireguard.interfaces = {
    wg0 = {
      # client ip
      ips = [ "10.66.66.2/24" "fd42:42:42::2/64" ];

      privateKeyFile = "/etc/wg-privkey"; # go away

      peers = [
        {
          # server's public key
          publicKey = "uPBpljWQp7cuKZ5sOiksuhf+sR/v/mdIjLA00Vluc2Q=";

          # server IP, and forward all traffic through VPN
          allowedIPs = [ "10.66.66.1/32" "fd42:42:42::1/128"
                         "0.0.0.0/0" "::/0" ];

          # the server
          endpoint = "45.79.140.85:51820";

          # keep the NAT alive
          persistentKeepalive = 25;
        }
      ];
    };
  };
}
