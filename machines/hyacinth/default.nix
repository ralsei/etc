# hyacinth -- system-specific settings
{ config, pkgs, lib, ... }:
{
  imports = [
    # <home-manager/nixos>
    ./hardware.nix
  ];

  # networking.firewall.allowedTCPPorts = [ 5555 5556 ];
  # networking.firewall.allowedUDPPorts = [ 5555 5556 ];

  # hostname and hostid (for zfs)
  networking.hostName = "hyacinth";
  networking.hostId = "3ae0d799";

  # quirks
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # systemd-boot
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

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
  networking.interfaces.wlp1s0.useDHCP = true;

  # audio
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
    # extraConfig = ''
    #  load-module module-switch-on-connect
    # '';
  };
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

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
  hazel.graphicalSession.enable = true;

  # various tools
  hazel.emacs = {
    enable = true;
    daemon = false; # eh
  };
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
  hazel.ctfTools.enable = false;

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
  programs.adb.enable = true;
  users.users.hazel.extraGroups = [ "docker" "adbusers" "dialout" ];
}
