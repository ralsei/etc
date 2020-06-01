{ config, pkgs, lib, ... }:
{
  # sway is managed by home-manager, BUT without this things break...
  programs.sway.enable = true;

  # allow setting GTK themes via home-manager. also generally useful despite
  # imo being mildly cursed
  programs.dconf.enable = true;

  # without this, swaylock does not unlock
  security.pam.services.swaylock = {
    text = ''
      auth include login
    '';
  };

  # idk
  fonts.fontconfig.allowBitmaps = true;
}
