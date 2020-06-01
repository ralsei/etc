{ stdenv, buildGoPackage, fetchFromGitHub }:
buildGoPackage rec {
  pname = "linx-client";
  version = "unstable";

  goPackagePath = "github.com/andreimarcu/${pname}";

  src = fetchFromGitHub {
    owner = "andreimarcu";
    repo = pname;
    rev = "11d4fecadbbc1c4efd29a8345a3dff8cb3b9a3c3";
    sha256 = "1i89mff4yp3w1g8lqnygybkmjzamvaxbpx7spwf89xvq5wl845cr";
  };

  goDeps = ./linx-client-deps.nix;

  buildFlags = [];
}
