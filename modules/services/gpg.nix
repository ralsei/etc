{ config, lib, pkgs, ... }:
{
  hazel.home = {
    services.gpg-agent.enable = true;

    home.packages = with pkgs; [
      pinentry-gtk2
    ];
  };
}
