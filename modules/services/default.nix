{ config, lib, pkgs, ... }:
{
  imports = [
    ./bitwarden.nix
    ./gitea.nix
    ./lemniscation.nix
    ./minecraft.nix
    ./mpd.nix
    ./nextcloud.nix
    ./nginx.nix
    ./perihelion.nix
    ./ssh.nix
  ];
}
