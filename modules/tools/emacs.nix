{ config, lib, pkgs, ... }:
with import <nixpkgs> {
  # grab the emacs overlay
  overlays = [
    (import (builtins.fetchTarball {
      url = "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
    }))
  ];
};

{
  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [
      epkgs.emacs-libvterm   # doom vterm module
    ];
    package = emacsUnstable; # emacs 27.0.91
  };

  # run the emacs daemon
  services.emacs.enable = true;

  programs.zsh = let
    emacsCmd = "emacsclient -c";
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
}
