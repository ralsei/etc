{ config, lib, pkgs, ... }:
let
  cfg = config.my.languages.ocaml;
in
with lib; {
  options = {
    my.languages.ocaml = {
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
    my.home = {
      home.packages = with pkgs; [
        ocaml
        m4

        # any other dependencies in a nix-shell
        ocamlPackages.base
        ocamlPackages.core
        ocamlPackages.async
        ocamlPackages.core_extended
        ocamlPackages.core_bench

        ocamlPackages.utop
        ocamlPackages.findlib
        ocamlPackages.merlin
        ocamlPackages.ocp-indent
      ];

      programs.opam.enable = true;
    };
  };
}
