{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.languages.scala;
in
with lib; {
  options = {
    hazel.languages.scala = {
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
    hazel.home = {
      home.packages = with pkgs; [
        scala
        jdk14

        sbt
        coursier
        metals
      ];
    };
  };
}
