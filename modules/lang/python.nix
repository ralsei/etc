{ config, lib, pkgs, ... }:
{
  home.packages = with pkgs; [
    python37
    python37Packages.pip
    python37Packages.ipython
    python37Packages.black
    python37Packages.setuptools
    python37Packages.pylint
    python37Packages.poetry
    python37Packages.pwntools
    hazel.jupyterWithBatteries
  ];
}
