{ config, lib, pkgs, ... }:
let
  cfg = config.my.languages.java;
in
with lib; {
  options.my.languages.java.enable = mkEnableOption "java";

  config = mkIf cfg.enable {
    my.home = {
      home.packages = with pkgs; [
        jdk
        gradle
      ];
    };
  };
}
