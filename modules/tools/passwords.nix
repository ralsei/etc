{ config, lib, pkgs, ... }:
{
  # haha you thought
  home.packages = with pkgs; [
    bitwarden-cli
    pass-wayland
    pinentry-gtk2

    age
    minisign
    gnupg
  ];
}
