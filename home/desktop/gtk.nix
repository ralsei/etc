{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.gtkTheme;
in
with lib; {
  options = {
    hazel.gtkTheme = {
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
    gtk.enable = true;
    gtk.font = {
      package = pkgs.source-sans-pro;
      name = "IBM Plex Sans 10";
    };
    gtk.iconTheme = {
      package = pkgs.papirus-icon-theme;
      name = "Papirus";
    };
    gtk.theme = {
      package = pkgs.ant-theme;
      name = "Ant";
    };
  };
}
