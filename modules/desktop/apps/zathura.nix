{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.desktop.zathura;
in
with lib; {
  options = {
    hazel.desktop.zathura = {
      enable = mkOption {
        default = false;
        type = with types; bool;
        description = ''
          Enable the Zathura PDF reader.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    hazel.home.programs.zathura = {
      enable = true;
      extraConfig = (builtins.readFile /etc/nixos/config/zathura/zathurarc);
    };
  };
}
