{ config, lib, pkgs, ... }:
{
  imports = [
    ./desktop.nix
    ./fonts.nix
    ./hardware-configuration.nix
    ./networking.nix
    ./power.nix
  ];
}
