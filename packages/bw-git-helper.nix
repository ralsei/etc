{ sources ? import ../nix/sources.nix
, pkgs, stdenv, buildGoModule, fetchFromGitHub }:
let
  spdx = lic: lic // {
    url = "https://spdx.org/licenses/${lic.spdxId}.html";
  };
in
buildGoModule rec {
  pname = "bw-git-helper";
  version = "unstable";

  src = sources.bw-git-helper;

  # not managed by niv
  modSha256 = "1ck6bagvf9sjrp79lpx2w7v451ykn6hr3csm9zkqz1rqqd2z0smj";
  vendorSha256 = "1x7iwd4ndcvcwyx8cxhkcwn5kwwf8nam47ln743wnvcrxi7q2604";

  buildInputs = with pkgs; [ bitwarden-cli ];

  meta = with stdenv.lib; {
    description = "A git credential helper using BitWarden as a backend";
    homepage = "https://github.com/tudurom/bw-git-helper";
    license = spdx { spdxId = "EUPL-1.2"; };
    maintainers = [ maintainers.hazel ];
    platforms = platforms.all;
  };
}
