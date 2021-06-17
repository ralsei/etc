{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.languages.lisp;
in
with lib; {
  options = {
    hazel.languages.lisp = {
      enable = mkOption {
        default = false;
        type = with types; bool;
        description = ''
          Support for the Common Lisp and Raquette(tm) programming languages.
        '';
      };
    };
  };

  config =
    let
      racketBinaries = [
        "drracket"
        "gracket"
        "gracket-text"
        "mred"
        "mred-text"
        "mzc"
        "mzpp"
        "mzscheme"
        "mztext"
        "pdf-slatex"
        "plt-games"
        "plt-help"
        "plt-r5rs"
        "plt-r6rs"
        "plt-web-server"
        "racket"
        "raco"
        "scribble"
        "setup-plt"
        "slatex"
        "slideshow"
        "swindle"
      ];

      racketBinPath = "/home/hazel/src/racket/racket/bin/";

      genScript = binName:
        pkgs.writeScriptBin binName ''
          #!/bin/sh

          export LD_LIBRARY_PATH=${pkgs.racket.LD_LIBRARY_PATH}
          exec ${racketBinPath + binName} "$@"
        '';
    in
    mkIf cfg.enable {
    hazel.home = {
      home.packages = with pkgs; [
        # common lisp
        sbcl # THIS IS A STRICTLY STEEL BANK HOUSEHOLD
        lispPackages.quicklisp
        rlwrap # for sbcl

        chez
        chicken
        guile

        # the good shit
        # unstable.racket
        z3 # for rosette
      ] ++ (map genScript racketBinaries);

      programs.zsh.shellAliases = { "sbcl" = "rlwrap sbcl"; };
    };
  };
}
