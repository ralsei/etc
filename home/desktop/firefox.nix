{ config, pkgs, lib, ...}:
{
  # sipp ramm
  programs.firefox = {
    enable = true;
    package = pkgs.firefox-wayland;

    profiles = {
      default = {
        isDefault = true;
        id = 0;
        settings = {
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        };
        userChrome = (builtins.readFile /etc/nixos/config/firefox/userChrome.css);
      };
    };
  };

  xdg.configFile."tridactyl/tridactylrc".source = /etc/nixos/config/tridactyl/tridactylrc;

  home.sessionVariables = { "BROWSER" = "firefox"; };

  home.packages = with pkgs; [ tridactyl-native ];
}
