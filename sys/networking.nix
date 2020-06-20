{ config, pkgs, lib, ... }:
let
  cfg = config.hazel.networking;
in
with lib; {
  options = {
    hazel.networking = {
      wifi = mkOption {
        default = false;
        type = with types; bool;
      };
    };
  };

  config = {
    networking.networkmanager = if cfg.wifi then {
      enable = true;
      wifi.backend = "iwd";
    } else {};
    networking.resolvconf.enable = true;

    # opennic: https://www.opennic.org
    #networking.nameservers = [ "172.98.193.42" "66.70.228.164"
    #                           "128.31.0.72" "147.135.113.37" ];

    # TODO figure this out
    #networking.extraHosts = ''
    #  10.66.66.3 qtp2t.club aster.qtp2t.club blog.qtp2t.club cloud.qtp2t.club git.qtp2t.club p.qtp2t.club vault.qtp2t.club
    #'';
  };
}
