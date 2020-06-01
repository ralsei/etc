{ config, lib, pkgs, ... }:
{
  gtk.enable = true;
  gtk.font = {
    package = pkgs.source-sans-pro;
    name = "Source Sans Pro 10";
  };
  gtk.iconTheme = {
    package = pkgs.papirus-icon-theme;
    name = "Papirus";
  };
  gtk.theme = {
    package = pkgs.ant-theme;
    name = "Ant";
  };
}
