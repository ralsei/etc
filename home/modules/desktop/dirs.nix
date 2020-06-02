{ config, lib, pkgs, ... }:
{
  xdg.enable = true;
  xdg.userDirs.enable = true;

  # don't know why I have to do this
  xdg.configFile."user-dirs.conf".text = lib.mkForce ''
    enabled=True
  '';

  xdg.userDirs.desktop = "\$HOME/.cache/junk";
  xdg.userDirs.documents = "$\HOME/usr/doc";
  xdg.userDirs.download = "\$HOME/tmp";
  xdg.userDirs.music = "\$HOME/usr/music";
  xdg.userDirs.pictures = "\$HOME/usr/img";
  xdg.userDirs.publicShare = "\$HOME/.cache/junk/pub";
  xdg.userDirs.templates = "\$HOME/.cache/junk/tmp";
  xdg.userDirs.videos = "\$HOME/usr/video";
}
