{ config, lib, pkgs, ... }:
{
  imports = [
    ./automount.nix
    ./hackthebox.nix
    ./networking.nix
    ./power.nix
    ./private.nix
    ./wireguard.nix
    ./yubikey.nix
  ];
}
