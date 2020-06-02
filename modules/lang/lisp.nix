{ config, lib, pkgs, ... }:
{
  home.packages = with pkgs; [
    # common lisp
    sbcl # THIS IS A STRICTLY STEEL BANK HOUSEHOLD
    lispPackages.quicklisp
    rlwrap # for sbcl

    # the good shit
    racket
  ];

  programs.zsh.shellAliases = { "sbcl" = "rlwrap sbcl"; };
}
