{ inputs, options, config, lib, pkgs, ... }:
let
  inherit (inputs) agenix;
  secretsDir = "/../secrets/${config.networking.hostName}";
  secretsFile = "${secretsDir}/secrets.nix";
in
with lib;
{
  environment.systemPackages = [
    agenix.defaultPackage.x86_64-linux
  ];

  age = {
    secrets =
      mapAttrs' (n: _: nameValuePair (removeSuffix ".age" n) {
        file = ./. + "${secretsDir}/${n}";
        mode = "0444";
      }) (import (./. + secretsFile));
    sshKeyPaths =
      (if config.services.openssh.enable then
          map (e: e.path) (lib.filter (e: e.type == "rsa" || e.type == "ed25519") config.services.openssh.hostKeys)
       else []) ++ [
         "/etc/agenix/id_ed25519"
       ];
  };
}
