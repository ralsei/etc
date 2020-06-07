{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.zathura;
in
with lib; {
  options = {
    hazel.zathura = {
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
    programs.zathura = {
      enable = true;
      extraConfig = (builtins.readFile /etc/nixos/config/zathura/zathurarc);
    };
  };
}
