# virtual -- system-specific settings
{ config, pkgs, lib, ... }:
{
  imports = [
    <home-manager/nixos>
    ./hardware.nix
  ];

  # hostname
  networking.hostName = "virtual";
  networking.hostId = "1e6a2425";

  # clear /tmp on reboot
  boot.cleanTmpDir = true;

  # enable ZFS support
  boot.supportedFilesystems = [ "zfs" ];
  services.zfs.trim.enable = true;
  services.zfs.autoScrub.enable = true;
  services.zfs.autoSnapshot.enable = true; # enabled on all but /nix

  # automount USB devices
  hazel.autoMount.enable = true;

  # enable sway (also need to enable it with home-manager)
  hazel.graphicalSession.enable = true;

  # swap caps lock to dual esc+ctrl (!!)
  services.interception-tools.enable = true;

  # enable home-manager system-specific settings
  home-manager.users.hazel = import ./home.nix;
}
