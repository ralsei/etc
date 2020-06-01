{ config, lib, pkgs, ... }:
{
  accounts.email = {
    maildirBasePath = "var/mail";
    certificatesFile = /home/hazel/var/certs/protonmail_bridge.pem;

    accounts.protonmail = {
      primary = true;
      address = "me@qtp2t.club";
      # couldn't figure out how to use pass here.
      # also storing my password in plaintext isn't really an issue when it only
      # works if it's on my machine.
      passwordCommand = "${pkgs.coreutils}/bin/cat /home/hazel/.config/mbsync-pass";
      userName = config.accounts.email.accounts.protonmail.address;

      imap = {
        host = "127.0.0.1";
        port = 1143;
        tls.enable = false;
      };
      mbsync = {
        enable = true;
        create = "both";
      };

      smtp = {
        host = "127.0.0.1";
        port = 1025;
        tls.useStartTls = true;
      };
    };
  };

  services.mbsync = {
    enable = true;
    frequency = "*-*-* *:0,30:00";
  };

  programs.mbsync.enable = true;

  home.packages = with pkgs; [
    protonmail-bridge
    unstable.mu
  ];
}
