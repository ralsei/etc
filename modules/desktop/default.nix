{ unstable, config, lib, pkgs, ... }:
let
  cfg = config.my.graphicalSession;
in
with lib; {
  imports = [
    ./apps
    ./settings

    ./gnome.nix
  ];

  options = {
    my.graphicalSession = {
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
    my.desktop.gnome.enable = true;
    my.desktop.fonts.enable = true;
    my.desktop.gtkTheme.enable = true;
    my.desktop.firefox.enable = true;

    programs.evolution = {
      enable = true;
      plugins = with pkgs; [ evolution-ews ];
    };
    programs.seahorse.enable = true;
    services.gnome.evolution-data-server.enable = true;

    programs.steam = {
      enable = true;
      remotePlay.openFirewall = true;
      dedicatedServer.openFirewall = true;
    };

    my.home = {
      home.packages = with pkgs; [
        betterdiscordctl
        bitwarden
        blender
        brightnessctl
        celluloid
        dino
        (discord.override { withOpenASAR = true; })
        evince
        gimp
        godot_4
        jabref
        jq
        kdenlive
        libreoffice-fresh
        mullvad-vpn
        mpv
        mumble
        obs-studio
        pavucontrol
        picard
        playonlinux # hell
        qbittorrent
        steam-run
        soulseekqt
        spotify
        wl-clipboard
        xboxdrv
        xorg.xrdb
        zoom-us
        zotero
      ];

      services.gnome-keyring.enable = true;
    };

    services.mullvad-vpn.enable = true;
    networking.firewall.allowedUDPPorts = [ 5901 8080 ];
  };
}
