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
    ./mako.nix
    ./sway.nix
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
      alacritty.enable = true;
      firefox.enable = true;
      gtkTheme.enable = true;
      mako.enable = true;
      sway.enable = true;
    };
  };
}
