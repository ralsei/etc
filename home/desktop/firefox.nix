{ config, pkgs, lib, ...}:
let
  cfg = config.hazel.firefox;
in
with lib; {
  options = {
    hazel.firefox = {
      enable = mkOption {
        default = false;
        type = with types; bool;
        description = ''
          Enable the Firefox web browser.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
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

    xdg.configFile."tridactyl/tridactylrc".source =
      /etc/nixos/config/tridactyl/tridactylrc;

    home.sessionVariables = { "BROWSER" = "firefox"; };

    home.packages = with pkgs; [ tridactyl-native ];
  };
}
