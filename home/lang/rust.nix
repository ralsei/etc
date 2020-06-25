{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.lang.rust;
in
with lib; {
  options = {
    hazel.lang.rust = {
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
    home.packages = with pkgs; [
      rustup
      carnix
      unstable.rust-analyzer
    ];
  };
}
