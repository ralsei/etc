{ config, lib, pkgs, ... }:
{
  imports = [
    ./bitwarden.nix
    ./gitea.nix
    ./gpg.nix
    ./linx.nix
    ./mail.nix
    ./mpd.nix
    ./nextcloud.nix
    ./nginx.nix
    ./perihelion.nix
    ./ssh.nix
  ];
}