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
        owner = "hazel"; # no root secrets
        mode = "0444";
      }) (import (./. + secretsFile));
    sshKeyPaths = options.age.sshKeyPaths.default ++ [
      "/home/hazel/.ssh/id_ed25519"
      "/home/hazel/.ssh/id_rsa"
    ];
  };
}
