{ inputs, config, lib, pkgs, ... }:
{
  my.home = {
    programs.zsh = {
      enable = true;
      initExtra = (builtins.readFile ../../config/zsh/zshrc);
      profileExtra = (builtins.readFile ../../config/zsh/zprofile);

      defaultKeymap = "viins";

      plugins =
        [
          { name = "zsh-syntax-highlighting";
            src = inputs.zsh-syntax-highlighting; }
        ];

      sessionVariables = {
        "PAGER" = "less";
      };

      shellAliases = {
        "ls" = "eza --git --extended -h";
        "la" = "eza --git --extended -a";
        "l" = "eza --grid --git --extended -lh";
        "ll" = "eza --grid --git --extended -lah"; 
        "rm" = "rm -ir"; # saved my ass so many times

        "lc" = "linx-client";
        "q" = "exit";
        "x" = "clear";

        "rnb" = "toilet --gay -f future";
        "mtl" = "toilet --metal -f future";
        "rnt" = "toilet --gay -f term";

        "bitch" = "sudo";
        "nsw" = "sudo nixos-rebuild switch";
      };
    };

    programs.starship = {
      enable = true;
      settings = {
        add_newline = false;
        character = {
          success_symbol = "[λ](bold yellow)";
        };
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

    home.packages = with pkgs; [
      nix-zsh-completions
      eza
      fd
      htop
      tree
      toilet
    ];
  };
}
