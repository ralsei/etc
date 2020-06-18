{ config, lib, pkgs, ... }:
with lib; {
  # this is pretty much mandatory if I'm using sway
  config = mkIf config.hazel.sway.enable {
    xdg.configFile."wofi/config".text = ''
      width=400px
      term=alacritty
    '';

    xdg.configFile."wofi/style.css".text = ''
      window {
        background-color: #d5c4a1;
        margin: 10px;
        font-family: 'Source Code Pro';
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
}
