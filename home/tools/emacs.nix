{ config, lib, pkgs, ... }:
with import <nixos-unstable> {
  # grab the emacs overlay
  overlays = [
    (let
      sources = import /etc/nixos/nix/sources.nix;
      fetchGitHubArchive = { owner, repo, rev, sha256 }: builtins.fetchTarball {
        url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
        inherit sha256;
      };
    in import (fetchGitHubArchive {
      inherit (sources.emacs-overlay) owner repo rev sha256;
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
        epkgs.emacs-libvterm # doom vterm module
      ];
      package = emacsGcc;
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
      unstable.libgccjit
    ];
  };
}
