{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.desktop.mako;
in
with lib; {
  options = {
    hazel.desktop.mako = {
      enable = mkOption {
        default = false;
        type = with types; bool;
        description = ''
          Enable the Mako notification daemon.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    hazel.home = {
      programs.mako = {
        enable = true;

        font = "IBM Plex Mono 10";

        backgroundColor = "#cc241d";
        textColor = "#fbf1c7";
        progressColor = "over #fbf1c7";
        borderSize = 0;
        borderRadius = 10;

        margin = "20,20,10";
        padding = "10";

        defaultTimeout = 4000;
        maxVisible = 4;
        layer = "overlay";
        groupBy = "summary";
        maxIconSize = 40;
      };

      home.packages = with pkgs; [
        libnotify
      ];
    };
  };
}
