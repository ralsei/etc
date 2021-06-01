{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.languages.haskell;
in
with lib; {
  options = {
    hazel.languages.haskell.enable = mkEnableOption "haskell";
  };

  config = mkIf cfg.enable {
    hazel.home.home.packages = with pkgs; [
      ghc
      cabal2nix
      cabal-install
      stack
      haskellPackages.haskell-language-server
    ];
  };
}
