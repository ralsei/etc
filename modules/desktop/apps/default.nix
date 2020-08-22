{ config, lib, pkgs, ... }:
with lib; {
  imports = [
    ./alacritty.nix
    ./firefox.nix
    ./qutebrowser.nix
    ./zathura.nix
  ];
}
