{ config, pkgs, lib, ...}:
let
  cfg = config.hazel.services.mpd;

  mopidyEnv = pkgs.buildEnv {
    name = "mopidy-with-extensions-${pkgs.mopidy.version}";
    paths = lib.closePropagation (with pkgs; [
      mopidy-mpd
      mopidy-local
      mopidy-mpris
      mopidy-scrobbler
    ]);
    pathsToLink = [ "/${pkgs.mopidyPackages.python.sitePackages}" ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      makeWrapper ${pkgs.mopidy}/bin/mopidy $out/bin/mopidy \
        --prefix PYTHONPATH : $out/${pkgs.mopidyPackages.python.sitePackages}
    '';
  };
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
    };
  };


  config = mkIf cfg.enable {
    hazel.home = {
      # [HACK: Snow; 2022-07-24] fixed in HM master, update when 22.11 is out
      xdg.configFile."mopidy/mopidy.conf".source = config.age.secrets.lastFm.path;

      systemd.user.services.mopidy = {
        Unit = {
          Description = "mopidy music player daemon";
          Documentation = [ "https://mopidy.com/" ];
          After = [ "network.target" "sound.target" ];
        };

        Service = {
          ExecStart = "${mopidyEnv}/bin/mopidy";
        };

        Install.WantedBy = [ "default.target" ];
      };

      systemd.user.services.mopidy-scan = {
        Unit = {
          Description = "mopidy local files scanner";
          Documentation = [ "https://mopidy.com/" ];
          After = [ "network.target" "sound.target" ];
        };

        Service = {
          ExecStart =
            "${mopidyEnv}/bin/mopidy local scan";
          Type = "oneshot";
        };

        Install.WantedBy = [ "default.target" ];
      };
      
      # services.mpd = {
      #   enable = true;
      #   musicDirectory = /home/hazel/usr/music;

      #   # enable pulse and ncmpcpp visualizer
      #   extraConfig = ''
      #     audio_output {
      #       type "pulse"
      #       name "pulse audio"
      #     }

      #     audio_output {
      #       type "fifo"
      #       name "my_fifo"
      #       path "~/.local/share/mpd/fifo"
      #       format "44100:16:2"
      #     }
      #   '';
      # };

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
