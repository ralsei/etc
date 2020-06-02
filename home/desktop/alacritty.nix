{ config, pkgs, lib, ...}:
{
  programs.alacritty.enable = true;

  xdg.configFile."alacritty/alacritty.yml".source = lib.mkForce /etc/nixos/config/alacritty/alacritty.yml;
}
