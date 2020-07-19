# hyacinth -- system-specific settings
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

  # define all interfaces
  networking.useDHCP = false;
  networking.interfaces.enp0s25.useDHCP = true;
  networking.interfaces.wlan0.useDHCP = true;

  # audio
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # wifi
  hazel.networking.wifi = true;

  # automount USB devices
  hazel.autoMount.enable = true;

  # boppin
  hazel.yubikey = {
    enable = true;
    login = true;
  };

  # the pointing with the mouse and stuff
  hazel.graphicalSession = {
    enable = true;
    desktop = "sway";
  };
  hazel.desktop.sway = {
    outputs = {
      eDP-1 = {
        bg = "~/usr/img/papes/desktop/rose.jpg fill";
        res = "1920x1080";
        # pos = "1920 0";
      };
    };
    lockBg = "~/usr/img/papes/desktop/rose.jpg";
  };

  # various tools
  hazel.emacs.enable = true;
  hazel.langSupport.enable = true;
  hazel.services.mpd = {
    enable = true;
    mpris = true;
    scrobbling = true;
  };

  # laptop power adjustments
  hazel.laptopPower.enable = true;

  # wireguard vpn ip
  hazel.wireguard = {
    enable = true;
    addresses = [ "10.66.66.2/24" "fd42:42:42::2/64" ];
    routeAll = true;
  };

  # eyes emoji
  hazel.hackTheBox.enable = true;
  hazel.ctfTools.enable = true;

  # swap caps lock to dual esc+ctrl (!!)
  services.interception-tools.enable = true;
}
