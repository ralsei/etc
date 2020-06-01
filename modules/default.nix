{ config, lib, pkgs, ... }:
{
  imports = [
    ./desktop
    ./lang
    ./tools
  ];
}
