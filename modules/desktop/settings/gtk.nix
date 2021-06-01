{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.desktop.gtkTheme;
in
with lib; {
  options = {
    hazel.desktop.gtkTheme = {
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
    hazel.home.gtk = {
      enable = true;
      font = {
        package = null;
        name = "IBM Plex Sans 10";
      };
      iconTheme = {
        package = pkgs.papirus-icon-theme;
        name = "Papirus";
      };
      theme = {
        package = pkgs.matcha-gtk-theme;
        name = "Matcha-light-aliz";
      };
    };

    programs.dconf.enable = true;
  };
}
