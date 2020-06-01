{ pkgs, stdenv, fetchFromGitHub, rustPlatform }:
rustPlatform.buildRustPackage rec {
  pname = "zr";
  version = "0.9.0";

  src = fetchFromGitHub {
    owner = "jedahan";
    repo = pname;
    rev = version;
    sha256 = "15jcvasmm60mdhzrxw6dlbxjijh09n297h7wjbf3d3cdk4my2lgd";
  };

  cargoSha256 = "0cp4gpikrr51hif14h4gdrrsxxcrpzl6mg7k5sz7lfpw8mq4f8kc";

  buildInputs = with pkgs; [ pkgconfig openssl ];

  meta = with stdenv.lib; {
    description = "zsh plugin manager written in rust";
    homepage = "https://github.com/jedahan/zr/";
    license = licenses.mpl20;
    maintainers = [ maintainers.hazel ];
    platforms = platforms.all;
  };
}
