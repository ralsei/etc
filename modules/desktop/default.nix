{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.graphicalSession;
in
with lib; {
  imports = [
    ./apps
    ./core
    ./settings

    ./gnome.nix
    ./sway.nix
    ./hikari.nix
  ];

  options = {
    hazel.graphicalSession = {
      enable = mkOption {
        default = false;
        type = with types; bool;
        description = ''
          Enable a Wayland-based graphical session, and related apps.
        '';
      };

      desktop = mkOption {
        default = "gnome";
        type = with types; enum [ "gnome" "sway" "hikari" ];
      };

      menu = mkOption {
        default = "rofi";
        type = with types; enum [ "rofi" "wofi" ];
      };
    };
  };

  config = mkIf cfg.enable {
    hazel.desktop = {
      gnome.enable = cfg.desktop == "gnome";
      sway.enable = cfg.desktop == "sway";
      hikari.enable = cfg.desktop == "hikari";

      # mako.enable = true;
      # wofi.enable = true;
      # rofi.enable = true;

      fonts.enable = true;
      gtkTheme.enable = true;

      alacritty.enable = true;
      qutebrowser.enable = false;
      firefox.enable = true;
      zathura.enable = false;
    };

    hazel.home = {
      home.packages = with pkgs; [
        thunderbird
        pavucontrol
        mate.caja
        networkmanagerapplet
        imv
        mpv
        celluloid
        nextcloud-client
        libreoffice-fresh
        bitwarden
        evince

        dino
        unstable.element-desktop
        tdesktop
        mumble
        zoom-us
        teams
        ripcord

        gimp
        soulseekqt
        qbittorrent

        grim
        slurp
        wl-clipboard
        jq
        ponymix
        brightnessctl
        xorg.xrdb
      ];

      services.gnome-keyring.enable = true;
      services.nextcloud-client.enable = true;
    };
  };
}
