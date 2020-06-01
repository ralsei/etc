{ config, lib, pkgs, ... }:
{
  programs.zsh = {
    enable = true;
    initExtra = (builtins.readFile ../../config/zsh/zshrc);
    profileExtra = (builtins.readFile ../../config/zsh/zprofile);
  };

  programs.z-lua = {
    enable = true;
    enableAliases = true;
  };

  programs.skim = {
    enable = true;
    defaultCommand = "fd --type f";
  };

  home.packages = with pkgs; [
    nix-zsh-completions
    exa
    fd
    htop
    tree
    hazel.zr
  ];
}
