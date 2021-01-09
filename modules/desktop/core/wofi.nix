{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.desktop.wofi;
in
with lib; {
  options = {
    hazel.desktop.wofi = {
      enable = mkOption {
        default = false;
        type = with types; bool;
      };
    };
  };

  config = mkIf cfg.enable {
    hazel.home = {
      xdg.configFile."wofi/config".text = ''
        width=400px
        term=alacritty
      '';

      xdg.configFile."wofi/style.css".text = ''
        window {
          background-color: #d5c4a1;
          margin: 10px;
          font-family: 'IBM Plex Sans';
          padding: 10px;
          border: 2px solid #cc241d;
        }

        #input {
          background-color: rgba(0,0,0,0);
          border: 0;
          color: #3c3836;
        }

        #outer-box {
          background-color: rgba(255,0,0,0);
          border: 2px solid #cc241d;
        }

        #inner-box {
          background-color: #eddbb2;
        }

        #scroll {
          background-color: #d5c4a1;
        }

        #text {
          color: #3c3836;
        }

        #entry{
          background-color: rgba(0,0,0,0);
        }

        #text:selected {
          color: #cc241d;
        }
      '';

      home.packages = with pkgs; [ wofi ];
    };
  };
}
