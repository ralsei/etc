{ config, lib, pkgs, ... }:
{
  home.packages = with pkgs; [
    texlive.combined.scheme-medium
  ];

  home.file.".latexmkrc".text = ''
    $pdf_mode = 1;
    $pdflatex = 'xelatex --shell-escape -interaction=nonstopmode %O %S -file-line-error -synctex=1';
  '';
}
