{ sources ? import /etc/nixos/nix/sources.nix
, pkgs ? import sources.nixpkgs {}
, config, lib, ... }:
{  
  imports = [
    ./desktop
    ./lang
    ./shell
    ./tools
  ];

  # let home-manager manage itself
  programs.home-manager.enable = true;

  # graphical applications go in desktop/default.nix
  home.packages = with pkgs; [
    nix-index # for file searchin'
    neofetch
    tokei
    bitwarden-cli
    pass-wayland
    age
    minisign
    hazel.linx-client
  ];

  home.stateVersion = "20.03";
}
