# aster -- system-specific settings
{ config, pkgs, lib, ... }:
{
  imports = [
    ./hardware.nix
  ];

  # hostname
  networking.hostName = "aster";

  # enable the Raspberry Pi bootloader
  boot.loader = {
    grub.enable = false;
    raspberryPi = {
      enable = true;
      version = 4;
    };
  };
  boot.kernelPackages = pkgs.linuxPackages_rpi4;

  # clear /tmp on reboot
  boot.cleanTmpDir = true;

  # init interfaces
  networking.useDHCP = false;
  networking.interfaces.eth0.useDHCP = true;
  networking.interfaces.wlan0.useDHCP = false;

  # wireguard vpn
  hazel.wireguard = {
    enable = true;
    addresses = [ "10.66.66.3/24" "fd42:42:42::3/64" ];
    routeAll = true;
  };
  networking.firewall.allowedUDPPorts = [ 51820 ];

  # the gameing
  hazel.services.sshd.enable = true;
  hazel.services.minecraft.enable = true;

  # no
  hazel.graphicalSession.enable = false;
}
