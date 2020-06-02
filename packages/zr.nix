{ sources ? import ../nix/sources.nix
, pkgs, stdenv, fetchFromGitHub, rustPlatform }:
rustPlatform.buildRustPackage rec {
  pname = "zr";
  version = "0.9.0";

  src = sources.zr;

  # not managed by niv
  cargoSha256 = "0hk7d4m3qjbcjv6b158acxwq9p6k4qa2mnjg8i2gpb8ccvgqdxki";

  buildInputs = with pkgs; [ pkgconfig openssl ];

  meta = with stdenv.lib; {
    description = "zsh plugin manager written in rust";
    homepage = "https://github.com/jedahan/zr/";
    license = licenses.mpl20;
    maintainers = [ maintainers.hazel ];
    platforms = platforms.all;
  };
}
