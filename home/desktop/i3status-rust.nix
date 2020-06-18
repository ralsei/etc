{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.i3status-rust;
in
with lib; {
  imports = [
    ../modules/i3status-rust.nix
  ];

  options = {
    hazel.i3status-rust = {
      enable = mkOption {
        default = false;
        type = with types; bool;
        description = ''
          Enable the i3status-rust statusbar.
        '';
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

      blocks = [
        { name = "battery"; }
        { name = "maildir";
          settings = {
            interval = 180;
            inboxes = [ "/home/hazel/var/mail/protonmail/INBOX" ];
            threshold_warning = 1;
            threshold_critical = 10;
            display_type = "new";
          };
        }
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
