{ config, pkgs, lib, ...}:
let
  cfg = config.hazel.mpd;
in
with lib; {
  options = {
    hazel.mpd = {
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
    home.file.".ncmpcpp/config".source = /etc/nixos/config/ncmpcpp/config;
    programs.zsh.shellAliases = { "m" = "ncmpcpp"; };

    # mpris support
    services.mpdris2 = {
      enable = cfg.mpris;
      notifications = cfg.mpris;
    };

    # load the mpdscribble config. this is private.
    xdg.configFile."mpdscribble/mpdscribble.conf".source =
        /etc/nixos/config/mpdscribble/mpdscribble.conf;

    # create a systemd service for mpdscribble
    # https://github.com/MusicPlayerDaemon/mpdscribble/blob/master/systemd/user/mpdscribble.service.in
    systemd.user.services.mpdscribble = if cfg.scrobbling then {
      Unit = {
        Description = "Audio scrobbler for MPD";
        Documentation = [ "man:mpdscribble(1)" ];
        After = [ "mpd.service" ];
      };

      Service = {
        Type = "simple";
        ExecStart = "${pkgs.mpdscribble}/bin/mpdscribble --no-daemon --conf /home/hazel/.config/mpdscribble/mpdscribble.conf";
      };

      Install = {
        WantedBy = [ "multi-user.target" ];
      };
    } else {};

    home.packages = with pkgs; [
      hazel.ncmpcppWithVisualizer
    ] ++
    (if cfg.mpris then [
      mpdris2
      playerctl
    ] else []) ++
    (if cfg.scrobbling then [
      mpdscribble
    ] else []);
  };
}
