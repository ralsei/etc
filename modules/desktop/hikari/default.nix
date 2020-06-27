{ config, lib, pkgs, ... }:
with lib; let
  cfg = config.hazel.desktop.hikari;
  makoCfg = config.hazel.desktop.mako;
  mpdCfg = config.hazel.services.mpd;
  rofiCfg = config.hazel.desktop.rofi;
  wofiCfg = config.hazel.desktop.wofi;
  waybarCfg = config.hazel.desktop.waybar;

  replaceSubstringsAttrs = with builtins; str1: str2: replaces:
    let
      replaceSubstringAttrs = replace:
        let
          # abusing the fact that all hikari keybinding names are lowercase here.
          replaceRpl = s: replaceStrings ["RPL"] [(toLower (toString replace))] s;
        in {
        "${replaceRpl str1}" = "${replaceRpl str2}";
      };
    in foldl' (x: y: x // y) {} (map replaceSubstringAttrs replaces);
in {
  imports = [
    /etc/nixos/modules/custom/hikari.nix
    ./waybar.nix
  ];

  options = {
    hazel.desktop.hikari = {
      enable = mkOption {
        default = false;
        type = with types; bool;
      };
    };
  };

  config = mkIf cfg.enable {
    hazel.desktop.waybar.enable = true;

    hazel.modules.hikari = {
      enable = true;
      autoStart = "#!/bin/sh\n"
                  + (if waybarCfg.enable then "waybar &\n" else "")
                  + (if makoCfg.enable then "mako &\n" else "")
                  + (if mpdCfg.mpris then "mpDris2 &\n" else "");
      config = {
        ui = {
          border = 2;
          gap = 5;
          step = 100;
          font = "IBM Plex Mono 10";

          colorscheme = rec {
            # TODO: hex codes. this is *shit*.
            background = 0;
            foreground = 3946550;
            selected   = 4556168;
            grouped    = inactive;
            first      = inactive;
            conflict   = 14129441;
            insert     = 10000154;
            active     = 13378589;
            inactive   = 2631720;
          };
        };

        outputs = {
          "*" = {
            background = "/home/hazel/usr/img/papes/desktop/lol_furries.png";
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
          terminal = "alacritty";
          menu = if wofiCfg.enable then "wofi --show drun"
                 else if rofiCfg.enable then "rofi -show drun"
                 else throw "At least one menu must be enabled";

          volup = "volume up";
          voldn = "volume down";
          volmt = "volume mute";

          mictoggle = "ponymix --source toggle";
          mustoggle = "playerctl play-pause";

          kbdlight = "keyboard cycle";
          brightup = "backlight up";
          brightdn = "backlight down";

          scrotsel = "scrot sel";
          scrotfull = "scrot full";
          scrotselup = "scrot selup";
          scrotfullup = "scrot fullup";

          emacs = "emacsclient -c";
        };

        bindings = {
          keyboard = {
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

            "LA+Up" = "view-decrease-size-up";
            "LAS+Up" = "view-increase-size-up";
            "LA+Down" = "view-increase-size-down";
            "LAS+Down" = "view-decrease-size-down";
            "LA+Left" = "view-decrease-size-left";
            "LAS+Left" = "view-increase-size-left";
            "LA+Right" = "view-increase-size-right";
            "LAS+Right" = "view-decrease-size-right";
            "L+r" = "view-reset-geometry";

            "L+minus" = "view-toggle-maximize-vertical";
            "L+less" = "view-toggle-maximize-horizontal";
            "L+f" = "view-toggle-maximize-full";
            "L5-35" = "view-toggle-floating"; # 'AltGr +' for '~'
            "L+i" = "view-toggle-invisible";

            "LS+o" = "group-only";
            "LS+h" = "group-hide";
            "LS+u" = "group-raise";
            "LS+d" = "group-lower";
            "L+Tab" = "group-cycle-prev";
            "LS+Tab" = "group-cycle-next";
            "L+asciicircum" = "group-cycle-view-prev";
            "LS+asciicircum" = "group-cycle-view-next";
            "LS+Home" = "group-cycle-view-first";
            "LS+End" = "group-cycle-view-last";

            "L+l" = "mode-enter-layout";
            "L+s" = "mode-enter-sheet-assign";
            "L+g" = "mode-enter-group-assign";
            "L+m" = "mode-enter-mark-assign";
            "L+acute" = "mode-enter-mark-select";
            "LS+acute" = "mode-enter-mark-switch-select";
            "LCA+g" = "mode-enter-input-grab";

            "LS+Backspace" = "lock";
            "LCA+q" = "quit";
            "LCA+r" = "reload";

            "L+Return" = "action-terminal";
            "L+Space" = "action-menu";
            "LS+Space" = "action-emacs";

            "0+XF86AudioMute" = "action-volmt";
            "0+XF86AudioLowerVolume" = "action-voldn";
            "0+XF86AudioRaiseVolume" = "action-volup";
            "0+XF86AudioMicMute" = "action-mictoggle";
            "0+XF86Tools" = "action-kbdlight";
            "0+XF86MonBrightnessUp" = "action-brightup";
            "0+XF86MonBrightnessDown" = "action-brightdn";
            "S+XF86AudioRaiseVolume" = "action-mustoggle";

            "0+Print" = "action-scrotsel";
            "S+Print" = "action-scrotfull";
            "L+Print" = "action-scrotselup";
            "LS+Print" = "action-scrotfullup";
          } // (replaceSubstringsAttrs "L+RPL" "workspace-switch-to-sheet-RPL" (range 0 9))
            // (replaceSubstringsAttrs "A+FRPL" "vt-switch-to-RPL" (range 1 7))
            // (replaceSubstringsAttrs "LS+RPL" "view-pin-to-sheet-RPL" (range 0 9))
            // (replaceSubstringsAttrs "L+RPL" "view-move-RPL" ["Up" "Down" "Left" "Right"])
            // (replaceSubstringsAttrs "LS+RPL" "view-snap-RPL" ["Up" "Down" "Left" "Right"]);

          mouse = {
            "L+left" = "mode-enter-move";
            "L+right" = "mode-enter-resize";
          };
        };
      };
    };
  };
}
