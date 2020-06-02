{ config, lib, pkgs, ... }:
{
  imports = [
    ./git.nix
    ./tmux.nix
    ./zsh.nix
  ];
}
