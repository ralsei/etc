{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.services.sshd;
in
with lib; {
  options = {
    hazel.services.sshd = {
      enable = mkOption {
        default = false;
        type = with types; bool;
      };

      mosh = mkOption {
        default = false;
        type = with types; bool;
      };
    };
  };

  config = mkIf cfg.enable {
    services.openssh = {
      enable = true;
      ports = [ 22 ];
      openFirewall = true;

      passwordAuthentication = false;
      authorizedKeysFiles = [
        "/etc/ssh_keys/hyacinth"
      ];
    };

    programs.mosh.enable = cfg.mosh;
  };
}
