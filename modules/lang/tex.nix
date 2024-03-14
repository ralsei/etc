{ config, lib, pkgs, ... }:
let
  cfg = config.my.languages.tex;
in
with lib; {
  options = {
    my.languages.tex = {
      enable = mkOption {
        default = false;
        type = with types; bool;
        description = ''
          Support for the TeX typesetting engine.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    my.home = {
      home.packages = with pkgs; [
        texlive.combined.scheme-full
        pandoc
      ];

      home.file.".latexmkrc".text = ''
        $pdf_mode = 1;
        $pdflatex = 'xelatex --shell-escape -interaction=nonstopmode %O %S -file-line-error -synctex=1';
      '';

      # install stuff as user
      home.sessionVariables = {
        "TEXMFHOME" = "~/.texmf";
        "TEXMFVAR" = "~/.texmf-var";
        "TEXMFCONFIG" = "~/.texmf-config";
      };
    };
  };
}
