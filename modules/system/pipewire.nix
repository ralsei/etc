{ config, lib, pkgs, ... }:
let
  cfg = config.my.pipewire;
in
with lib; {
  options.my.pipewire.enable = mkEnableOption "pipewire";

  config = mkIf cfg.enable {
    hardware.pulseaudio.enable = false;

    services.pipewire = {
      enable = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };
      pulse.enable = true;

      
    };

    security.rtkit.enable = true;
  };
}
