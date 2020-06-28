{ config, lib, pkgs, ... }:
{
  imports = [
    ./emacs.nix
    ./mail.nix
    ./nix.nix
  ];
}
