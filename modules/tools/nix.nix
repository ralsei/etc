{ config, lib, pkgs, ... }:
{
  hazel.home = {
    services.lorri.enable = true;

    programs.direnv.enable = true;

    home.packages = with pkgs; [
      niv
    ];
  };
}
