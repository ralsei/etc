{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.languages.agda;
in
with lib; {
  options = {
    hazel.languages.agda.enable = mkEnableOption "agda";
  };

  config = mkIf cfg.enable {
    hazel.home.home.packages = with pkgs; [
      (agda.withPackages [ agdaPackages.standard-library ])
    ];
  };
}
