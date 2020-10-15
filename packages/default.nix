let
  sources = import ../nix/sources.nix;
in
[
  (self: super: with super; rec {
    hazel = {
      # my packages
      burpsuite = callPackage ./burpsuite.nix {};
      butt = callPackage ./butt.nix {};
      bw-git-helper = callPackage ./bw-git-helper.nix {};
      go-rice = callPackage ./go-rice.nix {};
      hikari = callPackage ./hikari.nix {};
      linx-client = callPackage ./linx-client {};
      linx-server = callPackage ./linx-server.nix {};
      papermc = callPackage ./papermc.nix {};
      rofi-wayland = callPackage ./rofi-wayland/wrapper.nix {};
      zr = callPackage ./zr.nix {};

      perihelion = callPackage (import sources.perihelion) {};
      ziodyne-blog = import sources.ziodyne-blog;

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

    unstable = import <nixos-unstable> {
      inherit config;
    };

    # avoid using this if you can...
    master = import sources.nixpkgs-master {
      inherit config;
    };

    # HACK: the release-20.03 branch of home-manager does not support the option
    # `programs.rofi.package`
    rofi = hazel.rofi-wayland;
  })
]
