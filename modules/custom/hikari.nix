{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.modules.hikari;
in
with lib; {
  options = {
    hazel.modules.hikari = {
      enable = mkEnableOption "hikari";

      config = mkOption {
        type = with types; attrs; # don't know if this needs to be more specific
        default = {
          ui = {
            border = 1;
            gap = 5;
            step = 100;
            font = "monospace 10";

            colorscheme = {
              background = "0x282C34";
              foreground = "0x000000";
              selected   = "0xF5E094";
              grouped    = "0xFDAF53";
              first      = "0xB8E673";
              conflict   = "0xED6B32";
              insert     = "0xE3C3FA";
              active     = "0xFFFFFF";
              inactive   = "0x465457";
            };
          };

          outputs = {
            "*" = {
              background = "PREFIX/share/backgrounds/hikari/hikari_wallpaper.png";
            };
          };

          layouts = {
            s = {
              scale = {
                min = 0.5;
                max = 0.75;
              };
              left = "single";
              right = "stack";
            };

            q = {
              scale = 0.75;
              top = "single";
              bottom = "queue";
            };

            n = {
              left = "single";
              right = {
                top = "single";
                bottom = {
                  right = "single";
                  left = {
                    bottom = "single";
                    top = "full";
                  };
                };
              };
            };

            f = "full";
            h = "stack";
            v = "queue";
            g = "grid";
          };

          actions = {
            terminal = "$TERMINAL";
          };

          bindings = {
            keyboard = {
              "L+0" = "workspace-switch-to-sheet-0";
              "L+1" = "workspace-switch-to-sheet-1";
              "L+2" = "workspace-switch-to-sheet-2";
              "L+3" = "workspace-switch-to-sheet-3";
              "L+4" = "workspace-switch-to-sheet-4";
              "L+5" = "workspace-switch-to-sheet-5";
              "L+6" = "workspace-switch-to-sheet-6";
              "L+7" = "workspace-switch-to-sheet-7";
              "L+8" = "workspace-switch-to-sheet-8";
              "L+9" = "workspace-switch-to-sheet-9";
              "L+numbersign" = "workspace-switch-to-sheet-alternate";
              "L+Period" = "workspace-switch-to-sheet-current";
              "L+j" = "workspace-switch-to-sheet-next";
              "L+k" = "workspace-switch-to-sheet-prev";
              "L+Comma" = "workspace-switch-to-sheet-next-inhabited";
              "LS+Comma" = "workspace-switch-to-sheet-prev-inhabited";
              "LSC+g" = "workspace-show-group";
              "LSC+i" = "workspace-show-invisible";
              "LSC+Period" = "workspace-show-all";
              "LC+n" = "workspace-cycle-next";
              "LC+p" = "workspace-cycle-prev";

              "LC+i" = "sheet-show-invisible";
              "LC+Period" = "sheet-show-all";
              "LC+g" = "sheet-show-group";

              "LA+r" = "layout-reset";
              "LA+Return" = "layout-restack-append";
              "LAS+Return" = "layout-restack-prepend";
              "L+Home" = "layout-cycle-view-first";
              "L+End" = "layout-cycle-view-last";
              "L+n" = "layout-cycle-view-next";
              "L+p" = "layout-cycle-view-prev";
              "L+x" = "layout-exchange-view-next";
              "LS+x" = "layout-exchange-view-prev";
              "LA+x" = "layout-exchange-view-main";

              "LS+0" = "view-pin-to-sheet-0";
              "LS+1" = "view-pin-to-sheet-1";
              "LS+2" = "view-pin-to-sheet-2";
              "LS+3" = "view-pin-to-sheet-3";
              "LS+4" = "view-pin-to-sheet-4";
              "LS+5" = "view-pin-to-sheet-5";
              "LS+6" = "view-pin-to-sheet-6";
              "LS+7" = "view-pin-to-sheet-7";
              "LS+8" = "view-pin-to-sheet-8";
              "LS+9" = "view-pin-to-sheet-9";
              "LS+numbersign" = "view-pin-to-sheet-alternate";
              "LS+Period" = "view-pin-to-sheet-current";
              "LS+j" = "view-pin-to-sheet-next";
              "LS+k" = "view-pin-to-sheet-prev";

              "L+u" = "view-raise";
              "L+d" = "view-lower";
              "L+o" = "view-only";
              "L+h" = "view-hide";
              "L+q" = "view-quit";
              "LS+n" = "view-cycle-next";
              "LS+p" = "view-cycle-prev";

              "L+Up" = "view-move-up";
              "L+Down" = "view-move-down";
              "L+Left" = "view-move-left";
              "L+Right" = "view-move-right";
              "LA+Up" = "view-decrease-size-up";
              "LAS+Up" = "view-increase-size-up";
              "LA+Down" = "view-increase-size-down";
              "LAS+Down" = "view-decrease-size-down";
            };
          };
        };
      };

      # TODO: make this a list? shell codegen?
      autoStart = mkOption {
        type = with types; str;
        default = "";
      };
    };
  };

  config = mkIf cfg.enable {
    hazel.home.xdg.configFile."hikari/hikari.conf".text = builtins.toJSON cfg.config;
    hazel.home.xdg.configFile."hikari/autostart".text = cfg.autoStart;

    hazel.home.home.packages = with pkgs; [
      unstable.hikari
      xwayland
      swaybg
    ];
  };
}
