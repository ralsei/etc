{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.yubikey;
  desktopEnabled = config.hazel.graphicalSession.enable;
in
with lib; {
  options.hazel.yubikey = {
    enable = mkEnableOption "yubikey";
    login = mkOption {
      default = false;
      type = with types; bool;
    };
  };

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

    environment.systemPackages = with pkgs; [ yubioath-flutter ]
                                 ++ (if desktopEnabled then [ pkgs.pinentry-gtk2 ]
                                     else [ pkgs.pinentry-curses ]);

    hazel.home.home.packages = with pkgs; [
      yubikey-manager
      yubikey-personalization
    ] ++ (if desktopEnabled then with pkgs; [
      yubikey-manager-qt
      yubikey-personalization-gui
    ] else []);

    hazel.home.home.file.".gnupg/gpg-agent.conf" = {
      text = if desktopEnabled then ''
        pinentry-program ${pkgs.pinentry-gtk2}/bin/pinentry
      '' else ''
        pinentry-program ${pkgs.pinentry-curses}/bin/pinentry
      '';
    };

    security.pam.yubico = {
      enable = cfg.login;
      mode = "challenge-response";
      control = "required"; # oh boy.
    };

    security.pam.services.swaylock.yubicoAuth = cfg.login;
    security.pam.services.hikari-unlocker.yubicoAuth = cfg.login;
  };
}
