{ config, lib, pkgs, ... }:
{
  imports = [
    ./bat.nix
    ./emacs.nix
    ./git.nix
    ./mail.nix
    ./nix.nix
    ./tmux.nix
    ./zsh.nix
  ];
}
