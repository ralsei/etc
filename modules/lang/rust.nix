{ inputs, config, lib, pkgs, ... }:
let
  cfg = config.my.languages.rust;
in
with lib; {
  options = {
    my.languages.rust = {
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
    my.home = {
      home.packages = with pkgs; [
        rustup
      ];
    };
  };
}
