{ config, lib, pkgs, ... }:
{
  xdg.configFile."bat/config".source = ../../config/bat/config;

  home.packages = with pkgs; [
    bat
  ];
}
