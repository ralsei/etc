{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.languages.ocaml;
in
with lib; {
  options = {
    hazel.languages.ocaml = {
      enable = mkOption {
        default = false;
        type = with types; bool;
        description = ''
          Support for the OCaml programming language.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    hazel.home = {
      home.packages = with pkgs; [
        ocaml
        opam
        dune
        m4

        ocamlPackages.utop
        ocamlPackages.base
        ocamlPackages.core
        ocamlPackages.merlin
      ];
    };
  };
}
