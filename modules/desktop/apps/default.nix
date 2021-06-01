{ config, lib, pkgs, ... }:
with lib; {
  imports = [
    ./alacritty.nix
    ./firefox.nix
    ./zathura.nix
  ];
}
