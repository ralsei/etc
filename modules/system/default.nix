{ config, lib, pkgs, ... }:
{
  imports = [
    ./automount.nix
    ./hackthebox.nix
    ./networking.nix
    ./pipewire.nix
    ./power.nix
    ./wireguard.nix
    ./yubikey.nix
  ];
}
