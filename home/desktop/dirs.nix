{ config, lib, pkgs, ... }:
{
  xdg.enable = true;
  xdg.userDirs.enable = true;

  # don't know why I have to do this
  xdg.configFile."user-dirs.conf".text = lib.mkForce ''
    enabled=True
  '';

  xdg.userDirs.desktop = "/home/hazel/.cache/junk";
  xdg.userDirs.documents = "$\HOME/usr/doc";
  xdg.userDirs.download = "/home/hazel/tmp";
  xdg.userDirs.music = "/home/hazel/usr/music";
  xdg.userDirs.pictures = "/home/hazel/usr/img";
  xdg.userDirs.publicShare = "/home/hazel/.cache/junk/pub";
  xdg.userDirs.templates = "/home/hazel/.cache/junk/tmp";
  xdg.userDirs.videos = "/home/hazel/usr/video";

  home.packages = with pkgs; [
    xdg_utils
  ];
}
