{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.lang.lisp;
in
with lib; {
  options = {
    hazel.lang.lisp = {
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
    home.packages = with pkgs; [
      # common lisp
      sbcl # THIS IS A STRICTLY STEEL BANK HOUSEHOLD
      lispPackages.quicklisp
      rlwrap # for sbcl

      # the good shit
      racket
    ];

    programs.zsh.shellAliases = { "sbcl" = "rlwrap sbcl"; };
  };
}
