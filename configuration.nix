# configuration.nix -- the glue
{ config, lib, pkgs, ... }:
{
  imports = [
    <home-manager/nixos>
    ./cachix.nix

    ./machines/current
    ./sys
  ];

  # clean up the nix store periodically
  nix = {
    autoOptimiseStore = true;
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 10d";
    };
    trustedUsers = [ "root" "hazel" ];
  };

  # unfortunately, I live here
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  time.timeZone = "America/Indiana/Indianapolis";

  nixpkgs.overlays = import /etc/nixos/packages;
  nixpkgs.config.allowUnfree = true; # sorry, Stallman

  # the bare minimum
  environment.systemPackages = with pkgs; [
    coreutils
    git
    killall
    unzip
    wget
    vim
    gnumake

    hazel.cachix
    hazel.cached-nix-shell
  ];

  # unfortunately for everyone, it's me
  users.mutableUsers = false; # build-vm
  users.users.hazel = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" "audio" "video" "networkmanager" ];
    shell = pkgs.zsh;
  };

  # enable home-manager for my user
  home-manager.useUserPackages = true; # build-vm
  home-manager.useGlobalPkgs = true;

  system.stateVersion = "20.03";
}
