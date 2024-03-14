{ config, lib, pkgs, ... }:
{
  imports = [
    ./emacs.nix
    ./nix.nix
  ];
}
