# hyacinth -- home configuration
{ config, lib, pkgs, ... }:
{
  imports = [
    /etc/nixos/home
  ];

  hazel = {
    # graphical session
    graphicalSession.enable = true;
    sway = {
      outputs = {
        eDP-1 = {
          bg = "~/usr/img/papes/desktop/NES.jpg fill";
          res = "1920x1080";
          # pos = "1920 0";
        };
        HDMI-A-1 = {
          bg = "~/usr/img/papes/desktop/pipes.png fill";
          res = "1920x1080";
          # pos = "0 0";
        };
      };
      lockBg = "~/usr/img/papes/desktop/NES.jpg";
    };

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

  # gamer
  home.packages = with pkgs; [ 
    appimage-run
    minecraft
  ];
}
