# hyacinth -- home configuration
{ config, lib, pkgs, ... }:
{
  imports = [
    /etc/nixos/home
  ];

  hazel = {
    # graphical session
    graphicalSession.enable = true;

    # tools
    emacs.enable = true;
    mail.enable = true;
    mpd = {
      enable = true;
      mpris = true;
      scrobbling = true;
    };

    langSupport.enable = true;
  };
}
