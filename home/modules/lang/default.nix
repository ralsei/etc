{ config, lib, pkgs, ... }:
{
  imports = [
    ./c.nix
    ./lisp.nix
    ./python.nix
    ./rust.nix
    ./scala.nix
    ./tex.nix
  ];
}
