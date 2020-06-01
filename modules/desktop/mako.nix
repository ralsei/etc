{ config, lib, pkgs, ... }:
{
  programs.mako = {
    enable = true;

    font = "FiraCode 9";

    backgroundColor = "#cc241d";
    textColor = "#fbf1c7";
    progressColor = "over #fbf1c7";
    borderSize = 0;

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
}
