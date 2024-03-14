{ config, lib, pkgs, ... }:
with lib; {
  imports = [
    ./firefox.nix
  ];
}
