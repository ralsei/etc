{ unstable, config, lib, pkgs, ... }:
let
  cfg = config.hazel.graphicalSession;
in
with lib; {
  imports = [
    ./apps
    ./settings

    ./gnome.nix
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
    };
  };

  config = mkIf cfg.enable {
    hazel.desktop.gnome.enable = true;
    hazel.desktop.fonts.enable = true;
    hazel.desktop.gtkTheme.enable = true;
    hazel.desktop.firefox.enable = true;

    hazel.home = {
      home.packages = with pkgs; [
        thunderbird
        pavucontrol
        networkmanagerapplet
        imv
        mpv
        celluloid
        nextcloud-client
        libreoffice-fresh
        bitwarden
        evince
        zotero

        dino
        element-desktop
        tdesktop
        mumble
        zoom-us
        teams
        obs-studio

        gimp
        soulseekqt
        qbittorrent

        wl-clipboard
        jq
        ponymix
        brightnessctl
        xorg.xrdb

        steam-run
        appimage-run
        xboxdrv

        minecraft
        mcrcon
        kdenlive
        rmview
        dolphin-emu
        melonDS
        playonlinux # hell

        mathematica
      ];

      services.gnome-keyring.enable = true;
      services.nextcloud-client.enable = true;
    };
  };
}
