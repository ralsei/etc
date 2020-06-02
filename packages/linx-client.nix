{ sources ? import ../nix/sources.nix
, stdenv, buildGoPackage, fetchFromGitHub }:
buildGoPackage rec {
  pname = "linx-client";
  version = "unstable";

  goPackagePath = "github.com/andreimarcu/${pname}";

  src = sources.linx-client;

  goDeps = ./linx-client-deps.nix;

  buildFlags = [];
}
