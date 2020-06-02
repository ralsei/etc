{ config, lib, pkgs, ... }:
{
  programs.zsh = {
    enable = true;
    initExtra = (builtins.readFile ../../config/zsh/zshrc);
    profileExtra = (builtins.readFile ../../config/zsh/zprofile);

    defaultKeymap = "viins";

    plugins = let
      sources = import ../../nix/sources.nix; # pull from niv
    in [
      { name = "zsh-syntax-highlighting";
        src = sources.zsh-syntax-highlighting; }
      { name = "geometry";
        src = sources.geometry; }
    ];

    sessionVariables = {
      "GEOMETRY_SEPARATOR" = " ";
      "GEOMETRY_STATUS_SYMBOL" = "λ";
      "GEOMETRY_STATUS_COLOR" = "11";
      "GEOMETRY_STATUS_SYMBOL_ERROR" = "λ";
      "GEOMETRY_STATUS_COLOR_ERROR" = "red";
      "GEOMETRY_PATH_COLOR" = "green";

      "PAGER" = "less";
    };
    
    shellAliases = {
      "ls" = "exa --group-directories-first --git --extended -h";
      "la" = "exa --group-directories-first --git --extended -a";
      "l" = "exa --group-directories-first --grid --git --extended -lh";
      "ll" = "exa --group-directories-first --grid --git --extended -lah";
      "cat" = "bat";
      "rm" = "rm -ir"; # saved my ass so many times

      "lc" = "linx-client";
      "q" = "exit";
      "x" = "clear";

      "rnb" = "toilet --gay -f future";
      "mtl" = "toilet --metal -f future";
      "rnt" = "toilet --gay -f term";

      "bitch" = "sudo";
    };
  };

  programs.z-lua = {
    enable = true;
    enableAliases = true;
  };

  programs.skim = {
    enable = true;
    defaultCommand = "fd --type f";
  };

  xdg.configFile."bat/config".source = ../../config/bat/config;

  home.packages = with pkgs; [
    nix-zsh-completions
    exa
    bat
    fd
    htop
    tree
    toilet
  ];
}
