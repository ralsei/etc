{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.desktop.i3status-rust;
in
with lib; {
  imports = [
    /etc/nixos/modules/custom/i3status-rust.nix
  ];

  options = {
    hazel.desktop.i3status-rust = {
      enable = mkEnableOption "i3status-rust";
      batteries = mkOption {
        default = [ "BAT0" ];
        type = with types; (listOf str);
      };
    };
  };

  config = mkIf cfg.enable {
    hazel.modules.i3status-rust = {
      enable = true;

      icons.name = "awesome";
      theme = {
        name = "solarized-dark";
        overrides = {
          idle_bg = "#cc241d";
          idle_fg = "#fbf1c7";
          info_bg = "#fbf1c7";
          info_fg = "#3c3836";
          good_bg = "#79740e";
          good_fg = "#fbf1c7";
          warning_bg = "#b57614";
          warning_fg = "#fbf1c7";
          critical_bg = "#9d0006";
          critical_fg = "#fbf1c7";
          separator = "";
        };
      };

      blocks = let
        mkBattery = x: {
          name = "battery";
          settings = { device = x; };
        };
      in (map mkBattery cfg.batteries) ++ [
        { name = "net";
          settings = {
            device = "wg0";
            speed_up = false;
            speed_down = false;
          };
        }
        { name = "net";
          settings = {
            device = "wlan0";
            ssid = true;
            signal_strength = true;
            ip = false;
            speed_up = false;
            speed_down = false;
          };
        }
        { name = "time";
          settings = {
            format = "%Y-%m-%d %I:%M";
          };
        }
      ];
    };
  };
}
