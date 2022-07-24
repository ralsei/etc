{ config, pkgs, lib, ...}:
let
  cfg = config.hazel.services.mpd;
in
with lib; {
  imports = [
    ../custom/mopidy.nix
  ];

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
    hazel.modules.mopidy = {
      enable = true;
      extensionPackages = with pkgs; [
        mopidy-mpd
        mopidy-mpris
        mopidy-local
        mopidy-scrobbler
      ];

      settings = {
        mpd = {
          hostname = "::";
        };

        file = {
          media_dirs = [
            "$XDG_MUSIC_DIR|Music"
          ];
          follow_symlinks = true;
          excluded_file_extensions = [
            ".html"
            ".zip"
            ".jpg"
            ".jpeg"
            ".png"
          ];
        };
        local = {
          media_dir = "$XDG_MUSIC_DIR";
          excluded_file_extensions = [
            ".html"
            ".zip"
            ".jpg"
            ".jpeg"
            ".png"
          ];
        };

        audio = {
          output = ''
              tee name=t ! queue ! autoaudiosink t.
               ! queue ! audio/x-raw,rate=44100,channels=2,format=S16LE
               ! udpsink host=localhost port=5555
            '';
        };
      };

      extraConfigFiles = [ config.age.secrets.lastFm.path ];
    };

    hazel.home = {
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
