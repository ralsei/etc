{ config, pkgs, lib, ...}:
let
  cfg = config.hazel.desktop.alacritty;
in
with lib; {
  options = {
    hazel.desktop.alacritty = {
      enable = mkOption {
        default = false;
        type = with types; bool;
        description = ''
          Enable the Alacritty terminal emulator.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    hazel.home.programs.alacritty = {
      enable = true;

      settings = {
        window.padding = {
          x = 10;
          y = 10;
        };

        font = let
          mkFontSetting = x: {
            family = "Julia Mono";
            style = x;
          };
        in {
          normal = mkFontSetting "Regular";
          bold = mkFontSetting "Bold";
          italic = mkFontSetting "Italic";

          size = 10.0;
        };

        colors = {
          primary = {
            background = "0xfbf1c7";
            foreground = "0x3c3836";
          };
          normal = {
            black = "0xfbf1c7";
            red = "0xcc241d";
            green = "0x98971a";
            yellow = "0xd79921";
            blue = "0x458588";
            magenta = "0xb16286";
            cyan = "0x689d6a";
            white = "0x7c6f64";
          };
          bright = {
            black = "0x928374";
            red = "0x9d0006";
            green = "0x79740e";
            yellow = "0xb57614";
            blue = "0x076678";
            magenta = "0x8f3f71";
            cyan = "0x427b58";
            white = "0x3c3836";
          };
        };

        cursor.style = "Underline";
      };
    };
  };
}
