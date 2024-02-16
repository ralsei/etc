# hyacinth -- system-specific settings
{ config, pkgs, lib, ... }:
{
  imports = [
    ./hardware.nix
  ];

  # hostname and hostid (for zfs)
  networking.hostName = "hyacinth";
  networking.hostId = "3ae0d799";

  # systemd-boot
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # clear /tmp on reboot
  boot.tmp.cleanOnBoot = true;

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
  hazel.pipewire.enable = true;
  hardware.bluetooth.enable = true;

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
  hazel.services.mpd.enable = true;

  # laptop power adjustments
  hazel.laptopPower.enable = true;

  # wireguard vpn ip
  hazel.wireguard = {
    enable = false;
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

    plugins = [ pkgs.interception-tools-plugins.caps2esc ];

    # sudo uinput -p -d /dev/input/event0
    udevmonConfig = ''
      - JOB: "${pkgs.interception-tools}/bin/intercept -g $DEVNODE | ${pkgs.interception-tools-plugins.caps2esc}/bin/caps2esc | ${pkgs.interception-tools}/bin/uinput -d $DEVNODE"
        DEVICE:
          NAME: AT Translated Set 2 keyboard
          EVENTS:
            EV_KEY: [KEY_CAPSLOCK, KEY_ESC]
    '';
  };

  # ok
  hardware.opengl.enable = true;
  hardware.opengl.driSupport32Bit = true;

  # sigh
  virtualisation.docker.enable = true;
  programs.adb.enable = true;
  users.users.hazel.extraGroups = [ "docker" "adbusers" "dialout" ];
}
