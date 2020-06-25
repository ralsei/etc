{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.languages.c;
in
with lib; {
  options = {
    hazel.languages.c = {
      enable = mkOption {
        default = false;
        type = with types; bool;
        description = ''
          Tools for the C and C++ programming languages.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    hazel.home = {
      home.packages = with pkgs; [
        clang
        bear
        gdb
        pwndbg
        cmake
        llvmPackages.libcxx
      ];

      home.sessionVariables = {
        "CC" = "clang";
        "CXX" = "clang++";
      };
    };
  };
}
