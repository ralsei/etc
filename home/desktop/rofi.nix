{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.rofi;
in
with lib; {
  options = {
    hazel.rofi = {
      enable = mkOption {
        default = false;
        type = with types; bool;
      };
    };
  };

  # this is pretty much mandatory if I'm using sway
  config = mkIf cfg.enable {
    programs.rofi = {
      enable = true;

      font = "Source Code Pro 10";
      scrollbar = false;
      terminal = "\${pkgs.alacritty}/bin/alacritty";
      borderWidth = 2;
      theme = "gruvbox-light-hard";
    };
  };
}
