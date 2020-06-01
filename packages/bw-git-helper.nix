{ pkgs, stdenv, buildGoModule, fetchFromGitHub }:
let
  spdx = lic: lic // {
    url = "https://spdx.org/licenses/${lic.spdxId}.html";
  };
in
buildGoModule rec {
  pname = "bw-git-helper";
  version = "unstable";

  src = fetchFromGitHub {
    owner = "tudurom";
    repo = pname;
    rev = "d68a8db52a38061a0c9a60b3f29d81413d2e4425";
    sha256 = "1p7q3k1gfszm9k9sm4gp87464lh8l703w9jk0jrlrqhprj4wn1np";
  };

  modSha256 = "1ck6bagvf9sjrp79lpx2w7v451ykn6hr3csm9zkqz1rqqd2z0smj";

  buildInputs = with pkgs; [ bitwarden-cli ];

  meta = with stdenv.lib; {
    description = "A git credential helper using BitWarden as a backend";
    homepage = "https://github.com/tudurom/bw-git-helper";
    license = spdx { spdxId = "EUPL-1.2"; };
    maintainers = [ maintainers.hazel ];
    platforms = platforms.all;
  };
}
