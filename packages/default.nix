let
  sources = import ../nix/sources.nix;
in
[
  (self: super: with super; rec {
    hazel = {
      perihelion = callPackage (import sources.perihelion) {};
      ziodyne-blog = import sources.ziodyne-blog;
    };
  })
]
