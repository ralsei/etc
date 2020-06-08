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

  # laptop power adjustments
  # hazel.laptopPower = {
  #   enable = true;
  #   sensors = ''
  #     hwmon /sys/devices/platform/thinkpad_hwmon/hwmon/hwmon2/temp1_input
  #   '';
  # };

  # swap caps lock to dual esc+ctrl (!!)
  services.interception-tools.enable = true;

  # enable home-manager system-specific settings
  home-manager.users.hazel = import ./home.nix;
}
