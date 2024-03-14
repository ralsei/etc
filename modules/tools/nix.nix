{ config, lib, pkgs, ... }:
{
  my.home = {
    programs.direnv = {
      enable = true;
      enableZshIntegration = true;
      nix-direnv.enable = true;
    };
  };
}
