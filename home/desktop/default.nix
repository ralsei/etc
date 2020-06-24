{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.graphicalSession;
in
with lib; {
  imports = [
    ./alacritty.nix
    ./dirs.nix
    ./firefox.nix
    ./gtk.nix
    ./i3status-rust.nix
    ./mako.nix
    ./rofi.nix
    ./sway.nix
    ./wofi.nix
    ./zathura.nix
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
    hazel = {
      sway.enable = true;
      i3status-rust.enable = true;
      mako.enable = true;

      rofi.enable = true;
      # wofi.enable = true;

      gtkTheme.enable = true;

      alacritty.enable = true;
      firefox.enable = true;
      zathura.enable = true;
    };

    home.packages = with pkgs; [
      thunderbird
      pavucontrol
      mate.caja
      networkmanagerapplet
      imv
      mpv
      nextcloud-client

      dino
      riot-desktop
      tdesktop
      # (trying to get rid of) discord
      mumble

      gimp
      ghidra-bin
    ];

    services.gnome-keyring.enable = true;
    services.nextcloud-client.enable = true;
  };
}
