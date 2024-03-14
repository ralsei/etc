{ config, lib, pkgs, ... }:
let
  cfg = config.my.desktop.gnome;
in
with lib; {
  options.my.desktop.gnome.enable = mkEnableOption "gnome";

  config = mkIf cfg.enable {
    services.xserver = {
      enable = true;
      displayManager.gdm.enable = true;
      desktopManager.gnome.enable = true;
    };

    environment.gnome = {
      excludePackages = with pkgs; [
        gnome-photos
        gnome.geary
        gnome.gnome-maps
        gnome-console
        gnome.totem
      ];
    };

    my.home.home.packages = with pkgs; [
      gnome.gnome-tweaks
      gnome.gnome-terminal
      evince
    ];

    xdg.portal.enable = true;
  };
}
