{ config, lib, pkgs, ... }:
with import <nixpkgs> {
  # grab the emacs overlay
  overlays = [
    (import (builtins.fetchTarball {
      url = "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
    }))
  ];
};

let
  cfg = config.hazel.emacs;
in
with lib; {
  options = {
    hazel.emacs = {
      enable = mkOption {
        default = false;
        type = with types; bool;
        description = ''
          Enable the one true text editor.
        '';
      };

      daemon = mkOption {
        default = cfg.enable;
        type = with types; bool;
        description = ''
          Run Emacs in the background.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    programs.emacs = {
      enable = true;
      extraPackages = epkgs: [
        epkgs.emacs-libvterm   # doom vterm module
      ];
      package = emacsUnstable; # emacs 27.0.91
    };

    # run the emacs daemon
    services.emacs.enable = cfg.daemon;

    programs.zsh = let
      emacsCmd = if cfg.daemon then "emacsclient -c" else "emacs";
    in {
      shellAliases = { "e" = emacsCmd; };
      sessionVariables = { "EDITOR" = emacsCmd; "VISUAL" = emacsCmd; };
    };

    home.packages = with pkgs; [
      (ripgrep.override { withPCRE2 = true; })
      gnutls
      imagemagick
      zstd
      aspell
      aspellDicts.en
      aspellDicts.en-computers
      aspellDicts.en-science
      sqlite
    ];
  };
}
