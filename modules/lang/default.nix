{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.langSupport;
in
with lib; {
  imports = [
    ./c.nix
    ./lisp.nix
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
      c.enable = true;
      lisp.enable = true;
      python.enable = true;
      rust.enable = true;
      scala.enable = true;
      tex.enable = true;
    };
  };
}
