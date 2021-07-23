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
      desktopManager.gnome.enable = true;
    };

    environment.gnome = {
      excludePackages = with pkgs; [
        gnome-photos
        gnome.cheese
        gnome.geary
        gnome.gnome-maps
        gnome.gnome-music
        # gnome.gnome-terminal
        gnome.totem
      ];
    };

    hazel.home.home.packages = with pkgs; [
      gnome.gnome-tweak-tool
      evince
    ];

    xdg.portal = {
      enable = true;
      extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];
      gtkUsePortal = true;
    };
  };
}
