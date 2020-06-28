{ sources ? import /etc/nixos/nix/sources.nix
, stdenv, buildGoPackage, fetchFromGitHub }:
buildGoPackage rec {
  pname = "linx-client";
  version = "unstable";

  goPackagePath = "github.com/andreimarcu/${pname}";

  src = sources.linx-client;

  goDeps = ./deps.nix;

  buildFlags = [];
}
