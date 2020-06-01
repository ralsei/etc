{ config, lib, pkgs, ... }:
{
  home.packages = with pkgs; [
    scala
    jdk

    sbt
    coursier
    metals
  ];
}
