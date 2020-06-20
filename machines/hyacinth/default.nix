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

  # enable sway (also need to enable it with home-manager)
  hazel.graphicalSession.enable = true;

  # laptop power adjustments
  hazel.laptopPower = {
    enable = true;
    sensors = ''
      hwmon /sys/devices/platform/thinkpad_hwmon/hwmon/hwmon3/temp1_input
    '';
  };

  # wireguard vpn ip
  hazel.wireguard = {
    enable = true;
    addresses = [ "10.66.66.2/24" "fd42:42:42::2/64" ];
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

  # enable home-manager system-specific settings
  home-manager.users.hazel = import ./home.nix;
}
