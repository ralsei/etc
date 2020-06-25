{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.desktop.rofi;
in
with lib; {
  options = {
    hazel.desktop.rofi = {
      enable = mkOption {
        default = false;
        type = with types; bool;
      };
    };
  };

  # this is pretty much mandatory if I'm using sway
  config = mkIf cfg.enable {
    hazel.home.programs.rofi = {
      enable = true;

      font = "IBM Plex Mono 10";
      scrollbar = false;
      terminal = "\${pkgs.alacritty}/bin/alacritty";
      borderWidth = 2;
      theme = "gruvbox-light-hard";
    };
  };
}
