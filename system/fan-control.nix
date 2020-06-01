{ config, pkgs, lib, ... }:
{
  boot.extraModprobeConfig = ''
    options thinkpad_acpi fan_control=1 experimental=1
  '';


  services.thinkfan = {
    enable = true;
    
    sensors = ''
      # Entries here discovered by:
      # find /sys/devices -type f -name "temp*_input"
      hwmon /sys/devices/platform/thinkpad_hwmon/hwmon/hwmon3/temp6_input
      hwmon /sys/devices/platform/thinkpad_hwmon/hwmon/hwmon3/temp3_input
      hwmon /sys/devices/platform/thinkpad_hwmon/hwmon/hwmon3/temp7_input
      hwmon /sys/devices/platform/thinkpad_hwmon/hwmon/hwmon3/temp4_input
      hwmon /sys/devices/platform/thinkpad_hwmon/hwmon/hwmon3/temp1_input
      hwmon /sys/devices/platform/thinkpad_hwmon/hwmon/hwmon3/temp5_input
      hwmon /sys/devices/platform/thinkpad_hwmon/hwmon/hwmon3/temp2_input
      hwmon /sys/devices/virtual/thermal/thermal_zone0/hwmon4/temp1_input
    '';

    levels = ''
      (0,     0,      42)
      (1,     40,     47)
      (2,     45,     52)
      (3,     50,     57)
      (4,     55,     62)
      (5,     60,     77)
      (7,     73,     93)
      (127,   85,     32767)
    '';
  };
}
