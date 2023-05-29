{ inputs, config, lib, pkgs, ... }:
let
  cfg = config.hazel.emacs;
in
with lib; {
  options = {
    hazel.emacs = {
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

    hazel.home = {
      programs.emacs = {
        enable = true;
        extraPackages = epkgs: [
          epkgs.vterm
          # [HACK: Vera; 2023-05-29] Pro8a8ly remove this if not using Coq.
          epkgs.proof-general
        ];
        package = pkgs.emacsPgtk;
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
