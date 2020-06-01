[
  (self: super: with super; {
    hazel = {
      bw-git-helper = (callPackage ./bw-git-helper.nix {});
      crate2nix =
        (callPackage
          (builtins.fetchTarball
            "https://github.com/kolloch/crate2nix/tarball/0.8.0") {});
      jupyterWithBatteries = super.jupyter.override {
        definitions = {
          python3 = let
            env = (super.python3.withPackages(ps: with ps; [
              numpy
              scipy
              matplotlib
              pandas
              pwntools
            ]));
          in {
            displayName = "Python 3";
            argv = [];
            language = "python";
            logo32 = "${env.sitePackages}/ipykernel/resources/logo-32x32.png";
            logo64 = "${env.sitePackages}/ipykernel/resources/logo-64x64.png";
          };
        };
      };
      ncmpcppWithVisualizer = super.ncmpcpp.override { visualizerSupport = true; };
      zr = (callPackage ./zr.nix {});
    };
  })
]
