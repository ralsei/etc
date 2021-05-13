{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.languages.java;
in
with lib; {
  options.hazel.languages.java.enable = mkEnableOption "java";

  config = mkIf cfg.enable {
    hazel.home = {
      home.packages = with pkgs; [
        jdk
        gradle
      ];
    };
  };
}
