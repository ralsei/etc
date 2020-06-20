{ config, lib, pkgs, ... }:
{
  imports = [
    ./bitwarden.nix
    ./gitea.nix
    ./linx.nix
    ./nextcloud.nix
    ./nginx.nix
    ./perihelion.nix
    ./ssh.nix
  ];
}
