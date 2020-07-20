# lucy -- home configuration
{ config, lib, pkgs, ... }:
{
  imports = [
    /etc/nixos/home
  ];

  hazel = {
    # graphical session
    graphicalSession.enable = true;
    sway.outputs = {
      eDP-1 = {
        bg = "~/usr/img/papes/desktop/lain.jpg fill";
        res = "1600x900";
        pos = "0 0";
      };
    };

    # tools
    emacs.enable = true;
    mail.enable = true;
    mpd = {
      enable = true;
      mpris = true;
      scrobbling = true;
    };
  };
}
