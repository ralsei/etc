{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.modules.hikari;
in
with lib; {
  options = {
    hazel.modules.hikari = {
      enable = mkEnableOption "hikari";

      config = mkOption {
        type = with types; attrs; # don't know if this needs to be more specific
      };

      # TODO: make this a list? shell codegen?
      autoStart = mkOption {
        type = with types; str;
        default = "";
      };
    };
  };

  config = mkIf cfg.enable {
    security.pam.services.hikari-unlocker.text = "auth include login";

    hazel.home.xdg.configFile."hikari/hikari.conf".text = builtins.toJSON cfg.config;
    hazel.home.xdg.configFile."hikari/autostart".text = cfg.autoStart;
    hazel.home.xdg.configFile."hikari/autostart".executable = true;

    hazel.home.home.packages = with pkgs; [
      unstable.hikari
      xwayland
      swaybg
    ];
  };
}
