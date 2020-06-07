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

  # laptop power adjustments
  hazel.laptopPower = {
    enable = true;
    sensors = ''
      hwmon /sys/devices/platform/thinkpad_hwmon/hwmon/hwmon2/temp1_input
    '';
  };

  # wireguard vpn ip
  networking.wg-quick.interfaces.wg0.address =
    [ "10.66.66.2/24" "fd42:42:42::2/64" ];

  # firm ware up daet
  services.fwupd.enable = true;

  # swap caps lock to dual esc+ctrl (!!)
  services.interception-tools.enable = true;

  # sigh
  virtualisation.docker.enable = true;
  users.users.hazel.extraGroups = [ "docker" ];

  # enable home-manager system-specific settings
  home-manager.users.hazel = import ./home.nix;
}
