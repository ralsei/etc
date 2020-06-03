{ config, lib, pkgs, ... }:
{
  imports = [
    ./desktop.nix
    ./fonts.nix
    ./hardware.nix
    ./networking.nix
    ./power.nix
    ./private.nix
  ];
}
