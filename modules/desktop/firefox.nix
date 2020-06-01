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
          "toolkit.legacyUserProfileCustomizations.stylesheets" = "true";
        };
        userChrome = (builtins.readFile ../../config/firefox/userChrome.css);
      };
    };
  };

  xdg.configFile."tridactyl/tridactylrc".source = ../../config/tridactyl/tridactylrc;

  home.packages = with pkgs; [
    tridactyl-native
  ];
}
