{ inputs, config, lib, pkgs, ... }:
let
  cfg = config.my.emacs;
in
with lib; {
  options = {
    my.emacs = {
      enable = mkOption {
        default = false;
        type = with types; bool;
      };

      daemon = mkOption {
        default = cfg.enable;
        type = with types; bool;
      };
    };
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [ inputs.emacs-overlay.overlay ];

    my.home = {
      programs.emacs = {
        enable = true;
        extraPackages = epkgs: [
          epkgs.vterm
          epkgs.proof-general
        ];
        package = pkgs.emacs-pgtk;
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
        (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
        sqlite
        parinfer-rust
      ];
    };
  };
}
