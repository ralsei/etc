{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.lang.c;
in
with lib; {
  options = {
    hazel.lang.c = {
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
}
