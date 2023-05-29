{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.languages.coq;
in
with lib; {
  options = {
    hazel.languages.coq = {
      enable = mkOption {
        default = false;
        type = with types; bool;
      };
    };
  };

  config = mkIf cfg.enable {
    hazel.home = {
      home.packages = with pkgs; [
        coq
      ];
    };
  };
}
