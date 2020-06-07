{ config, lib, pkgs, ... }:
{
  imports = [
    ./desktop.nix
    ./fonts.nix
    ./networking.nix
    ./power.nix
    ./private.nix
  ];
}
