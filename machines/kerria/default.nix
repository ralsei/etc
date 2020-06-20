# kerria -- system-specific settings
{ config, pkgs, lib, ... }:
{
  imports = [
    <home-manager/nixos>
    ./hardware.nix
  ];

  # hostname
  networking.hostName = "kerria";

  # grub2, with serial
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";
  boot.loader.grub.extraConfig = ''
    serial --speed=115200 --unit=0 --word=8 --parity=no --stop=1
    terminal_input serial
    terminal_output serial
  '';

  # clear /tmp on reboot
  boot.cleanTmpDir = true;

  # define all interfaces
  networking.useDHCP = false;
  networking.interfaces.enp1s0.useDHCP = true;
  networking.interfaces.enp2s0.useDHCP = true;

  # this computer IS the server...
  # TODO: iterate over nginx vhosts?
  networking.extraHosts = ''
    127.0.0.1 qtp2t.club www.qtp2t.club blog.qtp2t.club cloud.qtp2t.club git.qtp2t.club lemniscation.qtp2t.club ring.qtp2t.club p.qtp2t.club vault.qtp2t.club
  '';

  # wireguard vpn
  hazel.wireguard = {
    enable = true;
    addresses = [ "10.66.66.4/24" "fd42:42:42::4/64" ];
    routeAll = true;
  };
  networking.firewall.allowedUDPPorts = [ 51820 ];

  # servin'
  hazel.services = {
    sshd = {
      enable = true;
      mosh = true;
    };
    nginx = {
      enable = true;
      ssl = true;
    };

    gitea.enable = true;
    nextcloud.enable = true;
    linx.enable = true;
    bitwarden.enable = true;
    perihelion.enable = true;
  };

  # no
  hazel.graphicalSession.enable = false;

  # sigh
  virtualisation.docker.enable = true;
  users.users.hazel.extraGroups = [ "docker" ];

  # enable home-manager system-specific settings
  home-manager.users.hazel = import ./home.nix;
}