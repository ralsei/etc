{ config, pkgs, lib, ...}:
let
  cfg = config.hazel.desktop.firefox;
in
with lib; {
  options = {
    hazel.desktop.firefox = {
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
    hazel.home = {
      # sipp ramm
      programs.firefox = {
        enable = true;
        package = pkgs.wrapFirefox pkgs.firefox-unwrapped {
          forceWayland = true;
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
