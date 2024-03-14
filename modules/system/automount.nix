{ config, lib, pkgs, ... }:
let
  cfg = config.my.autoMount;
in
with lib; {
  options = {
    my.autoMount = {
      enable = mkOption {
        default = false;
        type = with types; bool;
        description = ''
          Enable automatic mounting of devices.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    services.gvfs.enable = true;
    services.udisks2.enable = true;
    services.devmon.enable = true;

    my.home = {
      services.udiskie = {
        enable = true;
        tray = "never";
      };

      home.packages = with pkgs; [ udiskie ];
    };
  };
}
