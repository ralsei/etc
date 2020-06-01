{ config, pkgs, ... }:
{  
  imports = [
    ./modules
  ];

  # let home-manager manage itself
  programs.home-manager.enable = true;

  nixpkgs.overlays = import ./packages;
  nixpkgs.config = {
    allowUnfree = true; # sorry, Stallman
    packageOverrides = pkgs: {
      unstable = import <nixos-unstable> {
        config = config.nixpkgs.config;
      };
    };
  };

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
    profanity    # xmpp
    dino         # also xmpp
    riot-desktop # may tricks
    tdesktop     # dash dash dot
    discord      # garbage

    # fetchin'
    neofetch
    tokei
    toilet # heehee hoohoo

    # encryptin'
    bitwarden-cli
    pass-wayland
    pinentry-gtk2
    age
    minisign
    gnupg
  ];

  home.stateVersion = "20.03";
}
