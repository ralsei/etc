{ config, pkgs, lib, ... }:
{
  # fan control modules
  boot.extraModprobeConfig = ''
    options thinkpad_acpi fan_control=1 experimental=1
  '';

  # # thinkfan fan controller
  # services.thinkfan = {
  #   enable = true;

  #   # i don't know what these mean but only the first one works
  #   sensors = ''
  #     tp_thermal /proc/acpi/ibm/thermal (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -40, 0, 0, 0, 0, 0)
  #   '';
  # };

  # battery optimizations
  services.tlp.enable = true;
  powerManagement.powertop.enable = true;
  services.upower.enable = true;

  environment.systemPackages = with pkgs; [ powertop ];
}
