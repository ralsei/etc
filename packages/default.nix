[
  (self: super: with super; {
    hazel = {
      bw-git-helper = (callPackage ./bw-git-helper.nix {});
      linx-client = (callPackage ./linx-client.nix {});
      zr = (callPackage ./zr.nix {});

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
              ipykernel
            ]));
          in {
            displayName = "Python 3";
            argv = [
              "${env.interpreter}"
              "-m"
              "ipykernel_launcher"
              "-f"
              "{connection_file}"
            ];
            language = "python";
            logo32 = "${env.sitePackages}/ipykernel/resources/logo-32x32.png";
            logo64 = "${env.sitePackages}/ipykernel/resources/logo-64x64.png";
          };
        };
      };
      ncmpcppWithVisualizer = super.ncmpcpp.override { visualizerSupport = true; };
    };

    unstable = import <nixos-unstable> { inherit config; };
  })
]
