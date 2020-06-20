let
  sources = import ../nix/sources.nix;
in
[
  (self: super: with super; {
    hazel = {
      # my packages
      bw-git-helper = callPackage ./bw-git-helper.nix {};
      go-rice = callPackage ./go-rice.nix {};
      linx-client = callPackage ./linx-client.nix {};
      linx-server = callPackage ./linx-server.nix {};
      zr = callPackage ./zr.nix {};

      perihelion = callPackage (import sources.perihelion) {};
      ziodyne-blog = callPackage (import (super.fetchgit {
        url = "https://git.qtp2t.club/hazel/blog";
        rev = "1d0e0d2890bd57b832b2f0547c87ca7c770fd2d7";
        sha256 = "1n9k4pz2yb0gn7dvr9x5jw9fd0l00a2z8q4vip9rcykzj34kh0nb";
        fetchSubmodules = true;
      })) {};


      # other peoples' packages, from source
      cachix = import sources.cachix;
      cached-nix-shell = callPackage (import sources.cached-nix-shell) {};

      # overridden packages
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

    # i try to avoid installing packages from unstable -- the two exceptions
    # are mu/mu4e (my mua) and emacs feature/native-comp, which depends on
    # unstable/libgccjit.
    unstable = import <nixos-unstable> { inherit config; };
  })
]
