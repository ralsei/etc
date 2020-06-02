{ config, lib, pkgs, ... }:
{
  home.packages = with pkgs; [
    clang
    bear
    gdb
    cmake
    llvmPackages.libcxx
  ];

  programs.zsh.sessionVariables = {
    "CC" = "clang";
    "CXX" = "clang++";
  };
}
