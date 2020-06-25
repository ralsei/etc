{ config, pkgs, lib, ... }:
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
  ]);

  extrafonts = (with pkgs; [
    dejavu_fonts
    noto-fonts
    roboto
    roboto-slab
    source-code-pro
    emacs-all-the-icons-fonts
  ]);

  cfg = config.hazel.graphicalSession;
in
with lib; {
  options = {
    hazel.graphicalSession = {
      enable = mkOption {
        default = false;
        type = with types; bool;
        description = ''
          Who cares?
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    # sway is managed by home-manager, BUT without this things break...
    programs.sway.enable = true;

    # allow setting GTK themes via home-manager. also generally useful despite
    # imo being mildly cursed
    programs.dconf.enable = true;

    # without this, swaylock does not unlock
    security.pam.services.swaylock = {
      text = ''
        auth include login
      '';
    };

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
