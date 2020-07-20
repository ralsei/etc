# lucy -- system-specific settings
{ config, pkgs, lib, ... }:
{
  imports = [
    <home-manager/nixos>
    ./hardware.nix
  ];

  # hostname
  networking.hostName = "lucy";

  # systemd-boot
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # clear /tmp on reboot
  boot.cleanTmpDir = true;

  # ifaces
  networking.useDHCP = false;
  networking.interfaces.enp0s25.useDHCP = true;
  networking.interfaces.wlp3s0.useDHCP = true;

  # wireless support, if needed
  hazel.networking.wifi = true;

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
