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
  networking.extraHosts = ''
    10.66.66.4 qtp2t.club www.qtp2t.club blog.qtp2t.club cloud.qtp2t.club git.qtp2t.club lemniscation.qtp2t.club ring.qtp2t.club p.qtp2t.club vault.qtp2t.club
    10.66.66.4 knightsofthelambdacalcul.us www.knightsofthelambdacalcul.us blog.knightsofthelambdacalcul.us cloud.knightsofthelambdacalcul.us git.knightsofthelambdacalcul.us lemniscation.knightsofthelambdacalcul.us ring.knightsofthelambdacalcul.us p.knightsofthelambdacalcul.us vault.knightsofthelambdacalcul.us mail.knightsofthelambdacalcul.us
  '';

  # the gameing
  hazel.services.sshd.enable = true;
  hazel.services.minecraft.enable = true;

  # no
  hazel.graphicalSession.enable = false;
}
