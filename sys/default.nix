{ config, lib, pkgs, ... }:
{
  imports = [
    ./automount.nix
    ./desktop.nix
    ./fonts.nix
    ./networking.nix
    ./power.nix
    ./private.nix
  ];
}
