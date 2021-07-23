{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.pipewire;
in
with lib; {
  options.hazel.pipewire.enable = mkEnableOption "pipewire";

  config = mkIf cfg.enable {
    hardware.pulseaudio.enable = false;

    services.pipewire = {
      enable = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };
      pulse.enable = true;
      media-session.enable = true;
    };

    security.rtkit.enable = true;
  };
}
