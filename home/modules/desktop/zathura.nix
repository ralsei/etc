{ config, lib, pkgs, ... }:
{
  programs.zathura = {
    enable = true;
    extraConfig = (builtins.readFile /etc/nixos/config/zathura/zathurarc);
  };
}
