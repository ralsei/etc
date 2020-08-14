{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.graphicalSession;
in
with lib; {
  imports = [
    ./apps
    ./core
    ./settings

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
        default = "sway";
        type = with types; enum [ "sway" "hikari" ];
      };

      menu = mkOption {
        default = "rofi";
        type = with types; enum [ "rofi" "wofi" ];
      };
    };
  };

  config = mkIf cfg.enable {
    hazel.desktop = {
      sway.enable = cfg.desktop == "sway";
      hikari.enable = cfg.desktop == "hikari";

      mako.enable = true;
      rofi.enable = true;

      fonts.enable = true;
      gtkTheme.enable = true;

      alacritty.enable = true;
      firefox.enable = true;
      zathura.enable = true;
    };

    hazel.home = {
      home.packages = with pkgs; [
        unstable.ungoogled-chromium # just in case
        thunderbird
        pavucontrol
        mate.caja
        networkmanagerapplet
        imv
        mpv
        nextcloud-client
        libreoffice-fresh

        dino
        unstable.element-desktop
        tdesktop
        mumble
        zoom-us

        gimp
        soulseekqt
        qbittorrent
        hazel.butt

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
