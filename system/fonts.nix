{ config, pkgs, lib, ... }:
let
  basefonts = (with pkgs; [
    fira-code
    corefonts
    font-awesome_4
    source-sans-pro
  ]);

  extrafonts = (with pkgs; [
    fira-code-symbol
    dejavu_fonts
    noto-fonts
    powerline-fonts
    roboto
    roboto-slab
    source-code-pro
    emacs-all-the-icons-fonts
  ]);
in
{
  # rock 'n roll
  # fonts.fonts = basefonts ++ [];

  fonts.fonts =
    basefonts ++
    extrafonts ++
    [];
}
