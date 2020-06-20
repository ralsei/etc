{ config, lib, pkgs, ... }:
{
  imports = [
    ./services

    ./automount.nix
    ./desktop.nix
    ./fonts.nix
    ./hackthebox.nix
    ./networking.nix
    ./power.nix
    ./private.nix
    ./wireguard.nix
  ];
}
