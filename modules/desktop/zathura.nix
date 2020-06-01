{ config, lib, pkgs, ... }:
{
  programs.zathura = {
    enable = true;
    extraConfig = (builtins.readFile ../../config/zathura/zathurarc);
  };
}
