{ config, lib, pkgs, ... }:
let
  cfg = config.my.languages.agda;
in
with lib; {
  options = {
    my.languages.agda.enable = mkEnableOption "agda";
  };

  config = mkIf cfg.enable {
    my.home.home.packages = with pkgs; [
      (agda.withPackages [ agdaPackages.standard-library ])
    ];
  };
}
