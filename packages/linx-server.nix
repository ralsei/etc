{ sources ? import ../nix/sources.nix
, pkgs, lib, stdenv, buildGoModule }:
buildGoModule rec {
  pname = "linx-server";
  version = "2.3.5";

  src = sources.linx-server;
  modSha256 = "1jjdh489ja7am1hpxnq6527zanivq1gzamn8ad69q1y1gd20inr7";

  nativeBuildInputs = with pkgs; [
    hazel.go-rice
  ];

  # horrible!
  postBuild = ''
    rice append --exec $GOPATH/bin/linx-server
  '';
  dontStrip = true;

  meta = with lib; {
    description = "Self-hosted file/code/media sharing website";
    homepage = "https://github.com/andreimarcu/${pname}";
    license = licenses.gpl3;
    maintainers = [ maintainers.hazel ];
  };
}
