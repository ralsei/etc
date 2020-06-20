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

  cfg = config.hazel.fonts;
in
with lib; {
  options = {
    hazel.fonts = {
      enable = mkOption {
        default = false;
        type = with types; bool;
      };
    };
  };

  config = mkIf cfg.enable {
    # rock 'n roll
    # fonts.fonts = basefonts ++ [];

    fonts.fonts =
      basefonts ++
      extrafonts ++
      [];
  };
}
