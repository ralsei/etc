{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.langSupport;
in
with lib; {
  imports = [
    ./agda.nix
    ./c.nix
    ./lisp.nix
    ./ocaml.nix
    ./python.nix
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
      lisp.enable = true;
      ocaml.enable = true;
      python.enable = true;
      rust.enable = true;
      scala.enable = true;
      tex.enable = true;
    };
  };
}
