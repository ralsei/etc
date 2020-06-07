{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.lang.scala;
in
with lib; {
  options = {
    hazel.lang.scala = {
      enable = mkOption {
        default = false;
        type = with types; bool;
        description = ''
          Support for the Scala programming language.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      scala
      jdk

      sbt
      coursier
      metals
    ];
  };
}
