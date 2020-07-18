{ config, lib, pkgs, ... }:
{
  imports = [
    ./ctf.nix
    ./emacs.nix
    ./mail.nix
    ./nix.nix
  ];
}
