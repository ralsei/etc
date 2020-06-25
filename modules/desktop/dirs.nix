{ config, lib, pkgs, ... }:
{
  hazel.home = {
    xdg.enable = true;
    xdg.userDirs.enable = true;

    xdg.userDirs.desktop = "/home/hazel/.cache/junk";
    xdg.userDirs.documents = "/home/hazel/usr/doc";
    xdg.userDirs.download = "/home/hazel/tmp";
    xdg.userDirs.music = "/home/hazel/usr/music";
    xdg.userDirs.pictures = "/home/hazel/usr/img";
    xdg.userDirs.publicShare = "/home/hazel/.cache/junk/pub";
    xdg.userDirs.templates = "/home/hazel/.cache/junk/tmp";
    xdg.userDirs.videos = "/home/hazel/usr/video";

    home.packages = with pkgs; [
      xdg_utils
    ];
  };
}
