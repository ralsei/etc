{ config, lib, pkgs, ... }:
{
  services.gpg-agent.enable = true;

  home.packages = with pkgs; [
    pinentry-gtk2
  ];
}
