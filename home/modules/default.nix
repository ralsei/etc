{ config, lib, pkgs, ... }:
{
  imports = [
    ./desktop
    ./lang
    ./shell
    ./tools
  ];
}
