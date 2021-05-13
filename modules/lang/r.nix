{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.languages.r;

  rPkgs = with pkgs.rPackages; [
    tidyverse
    colorspace
    corrr
    cowplot
    gapminder
    ggdark
    ggforce
    ggrepel
    ggridges
    ggsci
    ggthemes
    gridExtra
    gss
    patchwork
    rcartocolor
    scico
    socviz
    showtext
    shiny
    plotly
    highcharter
    echarts4r
  ];
in
with lib; {
  options.hazel.languages.r.enable = mkEnableOption "r";

  config = mkIf cfg.enable {
    hazel.home = {
      home.packages = with pkgs; [
        (rWrapper.override { packages = rPkgs; })
        (rstudioWrapper.override { packages = rPkgs; })

        rPackages.rmarkdown
        rPackages.knitr
      ];
    };
  };
}
