{ config, pkgs, lib, ...}:
let
  cfg = config.hazel.services.mpd;
in
with lib; {
  options = {
    hazel.services.mpd = {
      enable = mkOption {
        default = false;
        type = with types; bool;
        description = ''
          Enable the music player daemon.
        '';
      };

      mpris = mkOption {
        default = false;
        type = with types; bool;
        description = ''
          Enable MPRIS support for MPD.
        '';
      };

      scrobbling = mkOption {
        default = false;
        type = with types; bool;
        description = ''
          Enable the MPDScribble audio scrobbler.
        '';
      };
    };
  };


  config = mkIf cfg.enable {
    # create a systemd service for mpdscribble
    # https://github.com/MusicPlayerDaemon/mpdscribble/blob/master/systemd/user/mpdscribble.service.in
    systemd.user.services.mpdscribble = mkIf cfg.scrobbling {
      enable = true;
      description = "Audio scrobbler for MPD";
      after = [ "mpd.service" ];

      serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.mpdscribble}/bin/mpdscribble --no-daemon --conf /home/hazel/.config/mpdscribble/mpdscribble.conf";
        Restart = "on-failure";
      };

      wantedBy = [ "multi-user.target" ];
    };

    hazel.home = {
      services.mpd = {
        enable = true;
        musicDirectory = /home/hazel/usr/music;

        # enable pulse and ncmpcpp visualizer
        extraConfig = ''
          audio_output {
            type "pulse"
            name "pulse audio"
          }

          audio_output {
            type "fifo"
            name "my_fifo"
            path "~/.local/share/mpd/fifo"
            format "44100:16:2"
          }
        '';
      };

      # the client
      home.file.".ncmpcpp/config".source = ../../config/ncmpcpp/config;
      programs.zsh.shellAliases = { "m" = "ncmpcpp"; };

      # mpris support
      services.mpdris2 = {
        enable = cfg.mpris;
        notifications = false;
      };

      home.packages = with pkgs; [
        (ncmpcpp.override { visualizerSupport = true; })
        ashuffle
      ] ++
      (if cfg.mpris then [
        mpdris2
        playerctl
      ] else []) ++
      (if cfg.scrobbling then [
        mpdscribble
      ] else []);
    };
  };
}
