{ config, lib, pkgs, ... }:
let
  cfg = config.my.languages.haskell;
in
with lib; {
  options = {
    my.languages.haskell.enable = mkEnableOption "haskell";
  };

  config = mkIf cfg.enable {
    my.home.home.packages = with pkgs; [
      ghc
      cabal2nix
      cabal-install
      stack
      haskellPackages.haskell-language-server
    ];
  };
}
