# hyacinth -- system-specific settings
{ config, pkgs, lib, ... }:
{
  imports = [
    <home-manager/nixos>
    ./hardware.nix
  ];

  # hostname and hostid (for zfs)
  networking.hostName = "hyacinth";
  networking.hostId = "3ae0d799";

  # systemd-boot
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # AAAAAAAA
  boot.extraModulePackages = with config.boot.kernelPackages; [ v4l2loopback ];

  # clear /tmp on reboot
  boot.cleanTmpDir = true;

  # enable ZFS support
  boot.supportedFilesystems = [ "zfs" ];
  services.zfs.trim.enable = true;
  services.zfs.autoScrub.enable = true;
  services.zfs.autoSnapshot.enable = true; # enabled on all but /nix

  # define all interfaces
  networking.useDHCP = false;
  networking.interfaces.enp3s0f0.useDHCP = true;
  networking.interfaces.enp4s0.useDHCP = true;
  networking.interfaces.wlan0.useDHCP = true;

  # audio
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # wifi
  hazel.networking.wifi = true;

  # automount USB devices
  hazel.autoMount.enable = true;

  # the pointing with the mouse and stuff
  hazel.graphicalSession.enable = true;
  hazel.desktop.sway = {
    outputs = {
      eDP-1 = {
        bg = "~/usr/img/papes/desktop/lol_furries.png fill";
        res = "1920x1080";
        # pos = "1920 0";
      };
      HDMI-A-1 = {
        bg = "~/usr/img/papes/desktop/pipes.png fill";
        res = "1920x1080";
        # pos = "0 0";
      };
    };
    lockBg = "~/usr/img/papes/desktop/lol_furries.png";
  };

  # various tools
  hazel.emacs.enable = true;
  hazel.mail.enable = true;
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

  # enable hackthebox vpns
  hazel.hackTheBox.enable = true;

  # firm ware up daet
  services.fwupd.enable = true;

  # swap caps lock to dual esc+ctrl (!!)
  services.interception-tools = {
    enable = true;

    # sudo uinput -p -d /dev/input/event0
    # only works on laptop keyboard, since i need esc on hhkb
    udevmonConfig = ''
      - JOB: "intercept -g $DEVNODE | caps2esc | uinput -d $DEVNODE"
        DEVICE:
          NAME: AT Translated Set 2 keyboard
          EVENTS:
            EV_KEY: [KEY_CAPSLOCK, KEY_ESC]
    '';
  };

  # sigh
  virtualisation.docker.enable = true;
  users.users.hazel.extraGroups = [ "docker" ];

  # gamer
  hazel.home.home.packages = with pkgs; [
    appimage-run
    minecraft
  ];
}
