{ config, lib, pkgs, ... }:
let
  plex = pkgs.runCommand "ibm-plex" {
    src = pkgs.fetchzip {
      url = "https://github.com/IBM/plex/releases/download/v5.0.0/TrueType.zip";
      sha256 = "sha256-KKw9pk5YmWpaMKnYKhjwHynHxx8c0F8U/fgoU9qimHY=";
    };
  } "mkdir -p $out/share/fonts/truetype; cp $src/**/*.ttf $out/share/fonts/truetype";

  go-fonts = pkgs.runCommand "go-fonts" {
    src = pkgs.fetchzip {
      url = "https://go.googlesource.com/image/+archive/master/font/gofont/ttfs.tar.gz";
      sha256 = "sha256-rdzt51wY4b7HEr7W/0Ar/FB0zMyf+nKLsOT+CRSEP3o=";
      stripRoot = false;
    };
  } "mkdir -p $out/share/fonts/truetype; cp $src/*.ttf $out/share/fonts/truetype";

  cfg = config.my.desktop.fonts;
in
with lib; {
  options = {
    my.desktop.fonts = {
      enable = mkOption {
        default = false;
        type = with types; bool;
      };
    };
  };

  config = mkIf cfg.enable {
    fonts = {
      packages = with pkgs; [
        corefonts
        source-code-pro
        source-sans-pro
        font-awesome_4
        plex
        dejavu_fonts
        symbola
        julia-mono
        noto-fonts
        roboto
        roboto-slab
        emacs-all-the-icons-fonts
        go-fonts
      ];

      fontconfig = {
        enable = true;
        defaultFonts = {
          monospace = [ "Go Mono 10" ];
          sansSerif = [ "IBM Plex Sans 10" ];
          serif = [ "IBM Plex Serif 10" ];
        };
      };
    };
  };
}
