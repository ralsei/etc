{ config, lib, pkgs, ... }:
{
  imports = [
    ./bitwarden.nix
    ./gitea.nix
    ./linx.nix
    ./mail.nix
    ./nextcloud.nix
    ./nginx.nix
    ./perihelion.nix
    ./ssh.nix
  ];
}
