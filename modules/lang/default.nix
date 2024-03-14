{ config, lib, pkgs, ... }:
let
  cfg = config.my.langSupport;
in
with lib; {
  imports = [
    ./agda.nix
    ./c.nix
    ./coq.nix
    ./haskell.nix
    ./java.nix
    ./lisp.nix
    ./ocaml.nix
    ./python.nix
    ./r.nix
    ./rust.nix
    ./scala.nix
    ./tex.nix
  ];

  options = {
    my.langSupport = {
      enable = mkOption {
        default = false;
        type = with types; bool;
        description = ''
          Wildcard for language support.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    my.languages = {
      agda.enable = false;
      c.enable = true;
      coq.enable = false;
      haskell.enable = false;
      java.enable = true; # I FEEL NOTHING BUT PAIN
      lisp.enable = true;
      ocaml.enable = false;
      python.enable = true;
      r.enable = false;
      rust.enable = true;
      scala.enable = false;
      tex.enable = true;
    };
  };
}
