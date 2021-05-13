{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.desktop.gnome;
in
with lib; {
  options.hazel.desktop.gnome.enable = mkEnableOption "gnome";

  config = mkIf cfg.enable {
    services.xserver = {
      enable = true;
      displayManager.gdm.enable = true;
      desktopManager.gnome3.enable = true;
    };

    environment.gnome3 = {
      excludePackages = with pkgs; [
        gnome-photos
        gnome3.cheese
        gnome3.geary
        gnome3.gnome-maps
        gnome3.gnome-music
        # gnome3.gnome-terminal
        gnome3.totem
      ];
    };

    hazel.home.home.packages = with pkgs; [
      gnome3.gnome-tweak-tool

      gnomeExtensions.paperwm
      gnomeExtensions.dash-to-dock
      gnomeExtensions.mpris-indicator-button

      evince
    ];
  };
}
