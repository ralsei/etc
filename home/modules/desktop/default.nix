{ config, lib, pkgs, ... }:
{
  imports = [
    ./alacritty.nix
    ./dirs.nix
    ./firefox.nix
    ./gtk.nix
    ./mako.nix
    ./mpd.nix
    ./sway.nix
    ./zathura.nix
  ];
}
