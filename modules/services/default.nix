{ config, lib, pkgs, ... }:
{
  imports = [
    ./mpd.nix
    ./ssh.nix
  ];
}
