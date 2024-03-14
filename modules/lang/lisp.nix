{ config, lib, pkgs, ... }:
let
  cfg = config.my.languages.lisp;
in
with lib; {
  options = {
    my.languages.lisp = {
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

      libPath = lib.makeLibraryPath (with pkgs; [
        cairo
        fontconfig
        glib
        gmp
        gtk3
        gsettings-desktop-schemas
        libedit
        libGL
        libGLU
        libjpeg
        libpng
        mpfr
        openssl
        pango
        poppler
        readline
        sqlite

        # expeditor?
        ncurses
      ]);

      racketBinPath = "/home/my/src/racket/racket/bin/";

      genScript = binName:
        pkgs.writeScriptBin binName ''
          #!/bin/sh

          export LD_LIBRARY_PATH=${libPath}
          exec ${racketBinPath + binName} "$@"
        '';
    in
    mkIf cfg.enable {
    my.home = {
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
