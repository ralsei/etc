{ config, lib, pkgs, ... }:
let
  plex = pkgs.runCommand "ibm-plex" {
    src = pkgs.fetchzip {
      url = "https://github.com/IBM/plex/releases/download/v5.0.0/TrueType.zip";
      sha256 = "sha256-KKw9pk5YmWpaMKnYKhjwHynHxx8c0F8U/fgoU9qimHY=";
    };
  } "mkdir -p $out/share/fonts/truetype; cp $src/**/*.ttf $out/share/fonts/truetype";

  basefonts = (with pkgs; [
    corefonts
    source-code-pro
    source-sans-pro
    font-awesome_4
    plex
    julia-mono
  ]);

  extrafonts = (with pkgs; [
    dejavu_fonts
    noto-fonts
    roboto
    roboto-slab
    source-code-pro
    emacs-all-the-icons-fonts
  ]);

  cfg = config.hazel.desktop.fonts;
in
with lib; {
  options = {
    hazel.desktop.fonts = {
      enable = mkOption {
        default = false;
        type = with types; bool;
      };
    };
  };

  config = mkIf cfg.enable {
    fonts = {
      fonts = basefonts ++ extrafonts ++ [];

      fontconfig = {
        enable = true;
        defaultFonts = {
          monospace = [ "IBM Plex Mono 10" ];
          sansSerif = [ "IBM Plex Sans 10" ];
          serif = [ "IBM Plex Serif 10" ];
        };
      };
    };
  };
}
