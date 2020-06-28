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
    networking.extraHosts = ''
      10.66.66.4 qtp2t.club www.qtp2t.club blog.qtp2t.club cloud.qtp2t.club git.qtp2t.club lemniscation.qtp2t.club ring.qtp2t.club p.qtp2t.club vault.qtp2t.club
      10.66.66.4 knightsofthelambdacalcul.us www.knightsofthelambdacalcul.us blog.knightsofthelambdacalcul.us cloud.knightsofthelambdacalcul.us git.knightsofthelambdacalcul.us lemniscation.knightsofthelambdacalcul.us ring.knightsofthelambdacalcul.us p.knightsofthelambdacalcul.us vault.knightsofthelambdacalcul.us mail.knightsofthelambdacalcul.us
    '';
  };
}
