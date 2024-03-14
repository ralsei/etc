{ config, pkgs, lib, ...}:
let
  cfg = config.my.services.mpd;
in
with lib; {
  options = {
    my.services.mpd = {
      enable = mkOption {
        default = false;
        type = with types; bool;
        description = ''
          Enable the music player daemon.
        '';
      };
    };
  };


  config = mkIf cfg.enable {
    my.home = {
      services.mopidy = {
        enable = true;
        extensionPackages = with pkgs; [
          mopidy-mpd
          mopidy-local
          mopidy-mpris
          mopidy-scrobbler
          mopidy-spotify        
        ];
        extraConfigFiles = [
          config.age.secrets.lastFm.path
        ];
      };

      # the client
      home.file.".ncmpcpp/config".source = ../../config/ncmpcpp/config;
      programs.zsh.shellAliases = { "m" = "ncmpcpp"; };

      home.packages = with pkgs; [
        (ncmpcpp.override { visualizerSupport = true; })
        ashuffle
      ];
    };
  };
}
