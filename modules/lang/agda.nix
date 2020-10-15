{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.languages.agda;
in
with lib; {
  options = {
    hazel.languages.agda.enable = mkEnableOption "agda";
  };

  config = mkIf cfg.enable {
    # for linking
    environment.systemPackages = with pkgs; [
      haskellPackages.Agda
      AgdaStdlib
    ];

    environment.pathsToLink = [ "/share/agda" ];

    hazel.home.home.file = {
      ".agda/standard-library.agda-lib".text = ''
        name: standard-library
        include: /run/current-system/sw/share/agda
      '';
      ".agda/libraries".text = ''
        /home/hazel/.agda/standard-library.agda-lib
      '';
      ".agda/defaults".text = ''
        standard-library
      '';
    };
  };
}
