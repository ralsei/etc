{ config, pkgs, lib, ...}:
{
  # the music player daemon
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

  # for playerctl and notifications
  services.mpdris2 = {
    enable = true;
    notifications = true;
  };

  # load the mpdscribble config. this is private.
  xdg.configFile."mpdscribble/mpdscribble.conf".source =
    ../../config/mpdscribble/mpdscribble.conf;

  # create a systemd service for mpdscribble
  # https://github.com/MusicPlayerDaemon/mpdscribble/blob/master/systemd/user/mpdscribble.service.in
  systemd.user.services.mpdscribble = {
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
  };

  # the client
  home.file.".ncmpcpp/config".source = ../../config/ncmpcpp/config;
  programs.zsh.shellAliases = { "m" = "ncmpcpp"; };

  home.packages = with pkgs; [
    hazel.ncmpcppWithVisualizer
    mpdscribble
    playerctl
    mpdris2
  ];
}
