{ pkgs, stdenv, fetchzip,
  features ? {
    gammacontrol = true;
    layershell   = true;
    screencopy   = true;
    xwayland     = true;
  }
}:
let
  pname = "hikari";
  version = "2.1.0";
in stdenv.mkDerivation {
  inherit pname version;

  src = fetchzip {
    url = "https://hikari.acmelabs.space/releases/${pname}-${version}.tar.gz";
    sha256 = "14jy0d69nx6q8l2wf4k4hpizalh17mi23qpmw09zrvzhmz65baw4";
  };

  nativeBuildInputs = with pkgs; [ pkgconfig bmake ];

  buildInputs = with pkgs; [
    cairo
    glib
    libevdev
    libinput
    libxkbcommon
    linux-pam
    pango
    pixman
    unstable.libucl
    wayland
    wayland-protocols
    wlroots
  ];

  enableParallelBuilding = true;

  # Must replace GNU Make by bmake
  buildPhase = with stdenv.lib; concatStringsSep " " (
    [ "bmake" "-j$NIX_BUILD_CORES" "PREFIX=$out" ]
    ++ optional stdenv.isLinux "WITH_POSIX_C_SOURCE=YES"
    ++ mapAttrsToList (feat: enabled:
         optionalString enabled "WITH_${toUpper feat}=YES"
       ) features
  );

  # Can't suid in nix store
  # Run hikari as root (it will drop privileges as early as possible), or create
  # a systemd unit to give it the necessary permissions/capabilities.
  patchPhase = ''
    substituteInPlace Makefile --replace '4555' '555'
  '';

  installPhase = ''
    bmake \
      PREFIX=$out \
      install
    runHook postInstall
  '';

  meta = with stdenv.lib; {
    description = "Stacking Wayland compositor which is actively developed on FreeBSD but also supports Linux";
    homepage    = "https://hikari.acmelabs.space";
    license     = licenses.bsd2;
    platforms   = platforms.linux ++ platforms.freebsd;
    maintainers = with maintainers; [ jpotier ];
  };
}
