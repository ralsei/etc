{ stdenv, fetchurl, jdk12, runtimeShell }:

let
  version = "2020.7";
  jar = fetchurl {
    name = "burpsuite.jar";
    url = "https://portswigger.net/burp/releases/download?product=community&version=${version}&type=Jar";
    sha256 = "1iry8zy7zi6z4gcz79cmm5fghj199r0lvsh6pdr40r18kr0dmvpm";
  };
  launcher = ''
    #!${runtimeShell}
    exec ${jdk12}/bin/java -jar ${jar} "$@"
  '';
in stdenv.mkDerivation {
  pname = "burpsuite";
  inherit version;
  buildCommand = ''
    mkdir -p $out/bin
    echo "${launcher}" > $out/bin/burpsuite
    chmod +x $out/bin/burpsuite
  '';

  preferLocalBuild = true;

  meta = {
    description = "An integrated platform for performing security testing of web applications";
    longDescription = ''
      Burp Suite is an integrated platform for performing security testing of web applications.
      Its various tools work seamlessly together to support the entire testing process, from
      initial mapping and analysis of an application's attack surface, through to finding and
      exploiting security vulnerabilities.
    '';
    homepage = https://portswigger.net/burp/;
    downloadPage = "https://portswigger.net/burp/freedownload";
    license = [ stdenv.lib.licenses.unfree ];
    platforms = jdk12.meta.platforms;
    hydraPlatforms = [];
    maintainers = with stdenv.lib.maintainers; [ bennofs ];
  };
}
