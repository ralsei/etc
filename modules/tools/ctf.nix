{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.ctfTools;
  desktopEnabled = config.hazel.graphicalSession.enable;
in
with lib; {
  options.hazel.ctfTools.enable = mkEnableOption "ctf tools";

  config = mkIf cfg.enable {
    programs.wireshark = {
      enable = true;
      package = if desktopEnabled then pkgs.wireshark-qt else pkgs.wireshark-cli;
    };
    virtualisation.docker.enable = true;
    users.users.hazel.extraGroups = [ "wireshark" "docker" ];

    hazel.home.home.packages = with pkgs; [
      nmap
      binutils
      python3Packages.binwalk-full
      dirb
      john
      afl
    ] ++ (if desktopEnabled then with pkgs; [
      xorg.xhost # docker container xorg
      ghidra-bin
      # hazel.burpsuite
    ] else []);
  };
}
