{ sources ? import ../nix/sources.nix
, lib, stdenv, buildGoModule }:
buildGoModule rec {
  pname = "go-rice";
  version = "1.0.0";

  src = sources.go-rice;

  modSha256 = "0m9qs81dx5p6kc192a114gnkzlpdwgcr1a1r07kin1x7b2ahzwd0";

  meta = with lib; {
    description = "A Go package that embeds static files into a binary";
    homepage = "https://github.com/GeertJohan/go.rice";
    license = licenses.bsd2;
    maintainers = [ maintainers.hazel ];
  };
}
