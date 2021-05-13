{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.languages.python;
in
with lib; {
  options = {
    hazel.languages.python = {
      enable = mkOption {
        default = false;
        type = with types; bool;
        description = ''
          Support for the Python programming language and Jupyter Notebook.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    hazel.home = {
      home.packages = with pkgs; [
        python38
        python38Packages.pip
        python38Packages.ipython
        python38Packages.black
        python38Packages.setuptools
        python38Packages.pylint
        python38Packages.poetry
        python38Packages.pwntools


        unstable.sage
        # hazel.jupyterWithBatteries
      ];
    };
  };
}
