{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.yubikey;
  desktopEnabled = config.hazel.graphicalSession.enable;
in
with lib; {
  options.hazel.yubikey.enable = mkEnableOption "yubikey";

  config = mkIf cfg.enable {
    services.udev.packages = with pkgs; [
      yubikey-personalization
      libu2f-host
    ];
    services.pcscd.enable = true;

    environment.shellInit = ''
      export GPG_TTY="$(tty)"
      gpg-connect-agent /bye
      export SSH_AUTH_SOCK="/run/user/$UID/gnupg/S.gpg-agent.ssh"
    '';
    programs = {
      ssh.startAgent = false;
      gnupg.agent = {
        enable = true;
        enableSSHSupport = true;
        pinentryFlavor = if desktopEnabled then "gtk2" else "curses";
      };
    };

    environment.systemPackages = with pkgs; [ yubioath-desktop ]
                                 ++ (if desktopEnabled then [ pkgs.pinentry-gtk2 ]
                                     else [ pkgs.pinentry-curses ]);

    hazel.home.xdg.configFile."gnupg/gpg-agent.conf" = {
      text = if desktopEnabled then ''
        pinentry-program ${pkgs.pinentry-gtk2}/bin/pinentry
      '' else ''
        pinentry-program ${pkgs.pinentry-curses}/bin/pinentry
      '';
    };
  };
}
