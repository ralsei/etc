{ lib, buildPythonPackage, fetchPypi, mopidy }:

buildPythonPackage rec {
  pname = "datashape";
  version = "0.4.7";

  src = fetchPypi {
    inherit pname version;
    sha256 = "14b2ef766d4c9652ab813182e866f493475e65e558bed0822e38bf07bba1a278";
  };

  propagatedBuildInputs = [ numpy multipledispatch python-dateutil ];

  meta = with lib; {
    homepage = "https://github.com/ContinuumIO/datashape";
    description = "A data description language";
    license = licenses.bsd2;
    maintainers = with maintainers; [ fridh ];
  };
}
