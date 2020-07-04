{ config, lib, pkgs, ... }:
{
  hazel.home = {
    programs.zsh = {
      enable = true;
      initExtra = (builtins.readFile /etc/nixos/config/zsh/zshrc);
      profileExtra = (builtins.readFile /etc/nixos/config/zsh/zprofile);

      defaultKeymap = "viins";

      plugins = let
        sources = import /etc/nixos/nix/sources.nix; # pull from niv
      in [
        { name = "zsh-syntax-highlighting";
          src = sources.zsh-syntax-highlighting; }
      ];

      sessionVariables = {
        "PAGER" = "less";
      };

      shellAliases = {
        "ls" = "exa --git --extended -h";
        "la" = "exa --git --extended -a";
        "l" = "exa --grid --git --extended -lh";
        "ll" = "exa --grid --git --extended -lah";
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

    programs.starship = {
      enable = true;
      settings = {
        add_newline = false;
        character = {
          symbol = "λ";
          style_success = "bold yellow";
          style_failure = "bold red";
        };
      };
    };

    programs.z-lua = {
      enable = true;
      enableAliases = true;
    };

    programs.skim = {
      enable = builtins.currentSystem != "aarch64-linux";
      defaultCommand = "fd --type f";
    };

    xdg.configFile."bat/config".source = /etc/nixos/config/bat/config;

    home.packages = with pkgs; [
      nix-zsh-completions
      exa
      bat
      fd
      htop
      tree
      toilet
    ];
  };
}
