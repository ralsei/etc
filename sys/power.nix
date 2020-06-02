{ config, pkgs, lib, ... }:
{
  # fan control modules
  boot.extraModprobeConfig = ''
    options thinkpad_acpi fan_control=1 experimental=1
  '';

  # thinkfan fan controller
  # services.thinkfan = {
  #   enable = true;

  #   # i don't know what these mean but only the first one works
  #   sensors = ''
  #     tp_thermal /proc/acpi/ibm/thermal (0, 0, 0, 0, 0, 0, 0, 0)
  #   '';   
 
  #   levels = ''
  #     (0,     0,      42)
  #     (1,     40,     47)
  #     (2,     45,     52)
  #     (3,     50,     57)
  #     (4,     55,     62)
  #     (5,     60,     77)
  #     (7,     73,     93)
  #     (127,   85,     32767)
  #   '';
  # };

  # battery optimizations
  services.tlp.enable = true;
  powerManagement.powertop.enable = true;
  services.upower.enable = true;

  environment.systemPackages = with pkgs; [ powertop ];
}
