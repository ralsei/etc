{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.langSupport;
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
    hazel.langSupport = {
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
    hazel.languages = {
      agda.enable = true;
      c.enable = true;
      coq.enable = true;
      haskell.enable = true;
      java.enable = true; # I FEEL NOTHING BUT PAIN
      lisp.enable = true;
      ocaml.enable = true;
      python.enable = true;
      r.enable = false;
      rust.enable = true;
      scala.enable = false;
      tex.enable = true;
    };

    hazel.home.home.packages = with pkgs; [ julia-stable-bin ];
  };
}
