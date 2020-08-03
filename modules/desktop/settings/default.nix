{ config, lib, pkgs, ... }:
with lib; {
  imports = [
    ./dirs.nix
    ./fonts.nix
    ./gtk.nix
  ];
}
