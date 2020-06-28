# aster -- system-specific settings
{ config, pkgs, lib, ... }:
{
  imports = [
    ./hardware.nix
  ];

  # hostname
  networking.hostName = "aster";

  # clear /tmp on reboot
  boot.cleanTmpDir = true;

  # wireguard vpn
  hazel.wireguard = {
    enable = true;
    addresses = [ "10.66.66.3/24" "fd42:42:42::3/64" ];
    routeAll = true;
  };
  networking.firewall.allowedUDPPorts = [ 51820 ];

  # the gameing
  hazel.services.minecraft.enable = true;

  # no
  hazel.graphicalSession.enable = false;
}
