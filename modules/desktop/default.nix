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

    programs.evolution = {
      enable = true;
      plugins = with pkgs; [ evolution-ews ];
    };
    programs.seahorse.enable = true;
    services.gnome.evolution-data-server.enable = true;

    hazel.home = {
      home.packages = with pkgs; [
        betterdiscordctl
        bitwarden
        brightnessctl
        celluloid
        dino
        discord
        dolphin-emu
        element-desktop
        evince
        gimp
        jq
        kdenlive
        libreoffice-fresh
        mathematica
        mcrcon
        melonDS
        mpv
        mumble
        networkmanagerapplet
        nextcloud-client
        obs-studio
        pavucontrol
        picard
        playonlinux # hell
        polymc
        ponymix
        qbittorrent
        rmview
        soulseekqt
        tdesktop
        wl-clipboard
        xboxdrv
        xorg.xrdb
        zoom-us
        zotero
      ];

      services.gnome-keyring.enable = true;
      services.nextcloud-client.enable = true;
    };
  };
}
