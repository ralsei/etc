{ inputs, config, lib, pkgs, ... }:
let
  cfg = config.hazel.languages.rust;
in
with lib; {
  options = {
    hazel.languages.rust = {
      enable = mkOption {
        default = false;
        type = with types; bool;
        description = ''
          Support for the Rust programming language.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    hazel.home = {
      home.packages = with pkgs; [
        rustup
      ];
    };
  };
}
