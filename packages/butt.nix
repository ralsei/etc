{ stdenv, fetchurl, libogg, libvorbis, pkg-config
, libopus, lame, flac, portaudio, dbus, autoreconfHook
, libsamplerate, fdk_aac, openssl, fltk, libjpeg }:
stdenv.mkDerivation rec {
  pname = "butt";
  version = "0.1.22";

  src = fetchurl {
    url = "mirror://sourceforge/butt/${pname}-${version}.tar.gz";
    sha256 = "0b7nc3b9bs62ah5azwy3gsv8mksgvr1sgjlpa7q07y4pkpcgrrvi";
  };

  buildInputs = [
    libogg libvorbis libopus lame flac
    portaudio libsamplerate fdk_aac
    openssl fltk dbus libjpeg
  ];

  nativeBuildInputs = [ pkg-config autoreconfHook ];

  meta = {
    description = "broadcast using this tool: an easy to use, multi OS streaming tool";
    homepage    = "https://sourceforge.net/projects/butt/";
    license     = stdenv.lib.licenses.gpl2;
    platforms   = stdenv.lib.platforms.unix;
    maintainers = with stdenv.lib.maintainers; [ hazel ];
  };
}
