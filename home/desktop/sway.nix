{ config, pkgs, lib, ... }:
let
  cfg = config.hazel.sway;
in
with lib; {
  options = {
    hazel.sway = {
      enable = mkOption {
        default = false;
        type = with types; bool;
        description = ''
          Enable the Sway Wayland compositor.
        '';
      };

      outputs = mkOption {
        default = {};
        type = with types; attrsOf (attrsOf str);
        description = ''
          The attribute set defining the outputs.
        '';
      };

      lockBg = mkOption {
        type = with types; str;
        description = ''
          The background for the lockscreen.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    # compositor of choice
    wayland.windowManager.sway = let
      colors = {
        bg = "#fbf1c7";
        fg = "#3c3837";
        hl = "#cc241d";
        mut = "#d5c4a1";
        blk = "#3c3836";
        urg = "#d79921";
        nil = "#000000";
      };

      workspaces = [
        "1: I "
        "2: II "
        "3: III "
        "4: IV "
        "5: V "
        "6: VI "
        "7: VII "
        "8: VIII "
      ];

      systemMode = "system: (x) exit (l) lock (h) suspend (r) reboot (s) shutdown (i) reload";
      launchMode = "launch: (b) firefox (e) emacs (f) files (m) ncmpcpp";

      locker = ''swaylock -f --clock --font "Source Code Pro" -i "${cfg.lockBg}" \
                   --effect-blur 5x5 --timestr "%I:%M" \
                   --text-color "${colors.fg}" --ring-color "${colors.mut}" \
                   --inside-color "${colors.bg}" --line-color "${colors.fg}" \
                   --key-hl-color "${colors.hl}" --bs-hl-color "${colors.urg}"'';

      # create a workspace bind from one workspace
      genWSBind = with builtins; (ws: let
        modifier = config.wayland.windowManager.sway.config.modifier;
        wsNum = substring 0 1 ws; # kind of a hack, don't go above single digits.
      in {
        "${modifier}+${wsNum}" = ''workspace "${ws}"'';
        "${modifier}+Shift+${wsNum}" = ''move container to workspace "${ws}"'';
      });
    in {
      enable = true;
      wrapperFeatures.gtk = true; # force wayland

      config = {
        startup = let
          mako = config.hazel.mako.enable;
          mpris = config.hazel.mpd.mpris;
        in [
          { command = "xrdb -load ~/etc/config/X/Xresources"; always = true; }
          { command = ''
            swayidle -w \
              timeout 300 '${locker}' \
              timeout 600 'swaymsg "output * dpms off"' \
                   resume 'swaymsg "output * dpms on"' \
              before-sleep '${locker}'
          ''; }
        ] ++
        (if mako then
          [ { command = "mako"; } ]
         else []) ++
        (if mpris then
          [ { command = "mpDris2"; } ] # required because mako doesn't load...
         else []);

        output = cfg.outputs;

        # ACKSHUALLY, these are useless, but I'm putting them here anyway
        modifier = "Mod4";
        terminal = "alacritty";
        menu = "wofi -S drun"; # script

        # not doing mkOptionDefault because eh. too much is custom.
        # execs are in PATH, so I shouldn't have to specify ${pkgs.package}...
        keybindings = let
          modifier = config.wayland.windowManager.sway.config.modifier;
          term = config.wayland.windowManager.sway.config.terminal;
          menu = config.wayland.windowManager.sway.config.menu;
        in {
          # laptop and media hotkeys
          "XF86AudioRaiseVolume"  = "exec volume up";
          "XF86AudioLowerVolume"  = "exec volume down";
          "XF86AudioMute"         = "exec volume mute";
          "XF86AudioMicMute"      = "exec ponymix --source toggle";
          "XF86Tools"             = "exec keyboard cycle";
          "XF86MonBrightnessUp"   = "exec backlight up";
          "XF86MonBrightnessDown" = "exec backlight down";

          "Shift+XF86AudioRaiseVolume" = "exec playerctl play-pause";
          "Shift+XF86AudioLowerVolume" = "exec playerctl next";

          "Print"                   = "exec scrot sel";
          "Shift+Print"             = "exec scrot full";
          "${modifier}+Print"       = "exec scrot selup";
          "${modifier}+Shift+Print" = "exec scrot fullup";

          # important
          "${modifier}+Space"   = "exec ${menu}";
          "${modifier}+Return"  = "exec ${term}";
          "${modifier}+q"       = "kill";
          "${modifier}+Shift+r" = "reload";

          # jump around
          "${modifier}+h" = "focus left";
          "${modifier}+j" = "focus down";
          "${modifier}+k" = "focus up";
          "${modifier}+l" = "focus right";

          "${modifier}+Shift+h" = "move left";
          "${modifier}+Shift+j" = "move down";
          "${modifier}+Shift+k" = "move up";
          "${modifier}+Shift+l" = "move right";

          # layouts
          "${modifier}+b"       = "splith";
          "${modifier}+v"       = "splitv";
          "${modifier}+e"       = "layout toggle split";
          "${modifier}+f"       = "fullscreen";
          "${modifier}+Shift+f" = "floating toggle";
          "${modifier}+a"       = "focus parent";
          "${modifier}+s"       = "layout stacking";
          "${modifier}+w"       = "layout tabbed";

          # scratchpad
          "${modifier}+Shift+backslash" = "move scratchpad";
          "${modifier}+backslash"       = "scratchpad show";

          # modes
          "${modifier}+r"               = ''mode "resize"'';
          "${modifier}+semicolon"       = ''mode "${launchMode}"'';
          "${modifier}+Shift+semicolon" = ''mode "${systemMode}"'';
        } // (builtins.foldl' (x: y: x // y) {} (map genWSBind workspaces));
        # ^ generate workspaces

        modes = {
          "resize" = {
            "h" = "resize shrink width 20px";
            "j" = "resize grow height 20px";
            "k" = "resize shrink height 20px";
            "l" = "resize grow width 20px";

            "Return" = ''mode "default"'';
            "Escape" = ''mode "default"'';
          };

          "${launchMode}" = {
            "b" = ''exec firefox; mode "default"'';
            "e" = ''exec emacsclient -c; mode "default"'';
            "m" = ''exec alacritty -e ncmpcpp; mode "default"'';
            "f" = ''exec caja; mode "default"'';

            "Return" = ''mode "default"'';
            "Escape" = ''mode "default"'';
          };

          "${systemMode}" = {
            "l" = ''exec ${locker}; mode "default";'';
            "h" = "exec systemctl suspend";
            "r" = "exec systemctl reboot";
            "s" = "exec systemctl shutdown";
            "x" = "exit";
            "i" = "reload";

            "Return" = ''mode "default"'';
            "Escape" = ''mode "default"'';
          };
        };

        gaps = {
          inner = 5;
          smartBorders = "on";
          smartGaps = true;
        };

        fonts = [ "Source Code Pro 9" ];
        colors = {
          focused = {
            background = colors.hl;
            text = colors.bg;
            indicator = colors.hl;
            border = colors.hl;
            childBorder = colors.hl;
          };
          focusedInactive = {
            background = colors.bg;
            text = colors.fg;
            indicator = colors.blk;
            border = colors.blk;
            childBorder = colors.blk;
          };
          # these are getting kind of long, huh...
          unfocused = config.wayland.windowManager.sway.config.colors.focusedInactive;
          urgent = {
            background = colors.urg;
            text = colors.blk;
            indicator = colors.urg;
            border = colors.urg;
            childBorder = colors.urg;
          };
          background = colors.hl;
        };

        bars = [{
          position = "top";
          fonts = [ "FontAwesome" "Source Code Pro 9" ];
          workspaceNumbers = false;
          trayOutput = "none";
          statusCommand = "i3status-rs ~/.config/i3status-rust/status.toml";
          extraConfig = "height 20";

          colors = {
            separator = colors.fg;
            background = colors.blk;
            statusline = colors.fg;
            focusedWorkspace = {
              background = colors.hl;
              text = colors.bg;
              border = colors.hl;
            };
            activeWorkspace = {
              background = colors.bg;
              text = colors.fg;
              border = colors.bg;
            };
            # ...
            inactiveWorkspace =
              (builtins.head config.wayland.windowManager.sway.config.bars).colors.activeWorkspace;
            bindingMode = {
              background = colors.blk;
              text = colors.bg;
              border = colors.blk;
            };
            urgentWorkspace = {
              background = colors.urg;
              text = colors.bg;
              border = colors.urg;
            };
          };
        }];
      };

      extraConfig = ''
        seat * hide_cursor 1500
      '';
    };

    home.packages = with pkgs; [
      unstable.swaylock-effects # lockscreen
      swayidle                  # locker
      xwayland                  # xorg compatibility
      i3status-rust             # bar

      grim                      # screenshots
      slurp                     # screenshot select
      wl-clipboard              # control c control v

      jq                        # processing sway's data
      ponymix                   # volume scripts
      brightnessctl             # take a wild guess
      xorg.xrdb                 # xresources
      xrq                       # for a script
    ];
  };
}
