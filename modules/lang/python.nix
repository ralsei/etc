{ inputs, config, lib, pkgs, ... }:
let
  cfg = config.my.languages.python;
in
with lib; {
  options = {
    my.languages.python = {
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
    my.home = {
      home.packages = with pkgs; [
        python3
        python3Packages.pip
        python3Packages.ipython
        python3Packages.black
        python3Packages.setuptools
        python3Packages.pylint
        python3Packages.pwntools

        sage
      ];
    };
  };
}
