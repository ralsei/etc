{ config, lib, pkgs, ... }:
with lib; {
  imports = [
    ./i3status-rust.nix
    ./mako.nix
    ./rofi.nix
    ./waybar.nix
    ./wofi.nix
  ];
}
