{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.desktop.waybar;
in
with lib; {
  options = {
    hazel.desktop.waybar = {
      enable = mkOption {
        default = false;
        type = with types; bool;
      };
    };
  };

  config = mkIf cfg.enable {
    hazel.home.home.packages = with pkgs; [ waybar ];

    hazel.home.xdg.configFile."waybar/config".source =
      /etc/nixos/config/waybar/config;
    hazel.home.xdg.configFile."waybar/style.css".source =
      /etc/nixos/config/waybar/style.css;
  };
}
