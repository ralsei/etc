{ config, lib, pkgs, ... }:
let
  cfg = config.my.desktop.gtkTheme;
in
with lib; {
  options = {
    my.desktop.gtkTheme = {
      enable = mkOption {
        default = false;
        type = with types; bool;
        description = ''
          Enable GTK+, and my themes.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    my.home.home.packages = with pkgs; [
      papirus-icon-theme
      matcha-gtk-theme
    ];

    programs.dconf.enable = true;
  };
}
