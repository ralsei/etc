{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.desktop.qutebrowser;
in
with lib; {
  options.hazel.desktop.qutebrowser.enable = mkEnableOption "qutebrowser";

  config = mkIf cfg.enable {
    hazel.home = {
      home.packages = with pkgs; [
        unstable.qutebrowser

        # bitwarden userscript
        keyutils
        python37Packages.tldextract
        python37Packages.pyperclip
      ];
      home.sessionVariables = { "BROWSER" = "qutebrowser"; };

      xdg.configFile."qutebrowser/config.py".source =
        /etc/nixos/config/qutebrowser/config.py;
    };
  };
}
