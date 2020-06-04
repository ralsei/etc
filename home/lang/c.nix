{ config, lib, pkgs, ... }:
{
  home.packages = with pkgs; [
    clang
    bear
    gdb
    cmake
    llvmPackages.libcxx
  ];

  home.sessionVariables = {
    "CC" = "clang";
    "CXX" = "clang++";
  };
}
