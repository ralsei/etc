{ config, pkgs, lib, ... }:
let
  basefonts = (with pkgs; [
    corefonts
    source-code-pro
    source-sans-pro
    font-awesome_4
  ]);

  extrafonts = (with pkgs; [
    dejavu_fonts
    noto-fonts
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
