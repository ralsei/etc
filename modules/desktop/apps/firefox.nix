{ config, pkgs, lib, ...}:
let
  cfg = config.my.desktop.firefox;
in
with lib; {
  options = {
    my.desktop.firefox = {
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
    my.home = {
      programs.firefox = {
        enable = true;
        package = pkgs.wrapFirefox pkgs.firefox-unwrapped {
          extraPolicies = {
            ExtensionSettings = {};
          };
        };

        profiles = {
          default = {
            isDefault = true;
            id = 0;
            settings = {
              "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
            };
          };
        };
      };

      home.sessionVariables = { "BROWSER" = "firefox"; };
    };
  };
}
