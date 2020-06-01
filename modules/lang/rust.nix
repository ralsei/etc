{ config, lib, pkgs, ... }:
{
  home.packages = with pkgs; [
    rustup
    hazel.crate2nix
  ];
}
