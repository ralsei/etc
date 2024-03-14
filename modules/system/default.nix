{ config, lib, pkgs, ... }:
{
  imports = [
    ./automount.nix
    ./networking.nix
    ./pipewire.nix
    ./power.nix
    ./yubikey.nix
  ];
}
