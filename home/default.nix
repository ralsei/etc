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

  # run the gpg agent
  services.gpg-agent.enable = true;

  # for things that would be stupid to modularize
  home.packages = with pkgs; [
    nix-index # for file searchin'

    # desktop apps
    firefox-wayland
    pavucontrol
    pcmanfm
    networkmanagerapplet
    imv
    mpv

    # chattin'
    dino         # xmpp
    riot-desktop # may tricks
    tdesktop     # dash dash dot
    discord      # garbage

    # fetchin'
    neofetch
    tokei

    # encryptin'
    bitwarden-cli
    pass-wayland
    pinentry-gtk2
    age
    minisign
    gnupg

    hazel.linx-client
    ghidra-bin # eyes emoji
  ];

  home.stateVersion = "20.03";
}
