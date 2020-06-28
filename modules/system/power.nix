{ config, pkgs, lib, ... }:
let
  cfg = config.hazel.laptopPower;
in
with lib; {
  options = {
    hazel.laptopPower = {
      enable = mkOption {
        default = false;
        type = with types; bool;
        description = ''
          Enable power adjustments for laptops.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    # fan control modules
    boot.extraModprobeConfig = ''
      options thinkpad_acpi fan_control=1 experimental=1
    '';

    # battery optimizations
    services.tlp.enable = true;
    powerManagement.powertop.enable = true;
    services.upower.enable = true;

    environment.systemPackages = with pkgs; [ powertop ];
  };
}
