{ config, lib, pkgs, ... }:
{
  imports = [
    ./emacs.nix
    ./gpg.nix
    ./mail.nix
    ./mpd.nix
    ./nix.nix
  ];
}
