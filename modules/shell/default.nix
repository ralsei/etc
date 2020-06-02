{ config, lib, pkgs, ... }:
{
  imports = [
    ./bat.nix
    ./git.nix
    ./tmux.nix
    ./zsh.nix
  ];
}
