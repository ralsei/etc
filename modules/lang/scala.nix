{ config, lib, pkgs, ... }:
let
  cfg = config.my.languages.scala;
in
with lib; {
  options = {
    my.languages.scala = {
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
    my.home = {
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
