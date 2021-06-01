{ sources ? import ../nix/sources.nix, lib, stdenv, appimageTools, fetchurl }:
appimageTools.wrapType2 {
  name = "icestudio";
  src = fetchurl {
    url = "https://github.com/FPGAwars/icestudio/releases/download/v0.5.0/icestudio-0.5.0-linux64.AppImage";
    sha256 = "1blsprpkvm0ws9b96gb36f0rbf8f5jgmw4x6dsb1kswr4ysf591s";
  };
  extraPkgs = [ ];
}
