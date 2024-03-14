{ config, lib, pkgs, ... }:
let
  cfg = config.my.languages.coq;
in
with lib; {
  options = {
    my.languages.coq = {
      enable = mkOption {
        default = false;
        type = with types; bool;
      };
    };
  };

  config = mkIf cfg.enable {
    my.home = {
      home.packages = with pkgs; [
        coq
      ];
    };
  };
}
