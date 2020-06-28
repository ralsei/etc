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
    };
  };
}
