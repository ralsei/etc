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

  config = mkIf cfg.enable {
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
        racket
      ];

      programs.zsh.shellAliases = { "sbcl" = "rlwrap sbcl"; };
    };
  };
}
