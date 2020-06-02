{ config, lib, pkgs, ... }:
{
  xdg.configFile."bat/config".source = /etc/nixos/config/bat/config;

  home.packages = with pkgs; [
    bat
  ];
}
