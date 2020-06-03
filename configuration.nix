{ config, pkgs, lib, ... }:
{
  imports =
    [
      <home-manager/nixos>
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
  };

  # systemd-boot
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # clear /tmp on reboot
  boot.cleanTmpDir = true;

  # enable ZFS support
  boot.supportedFilesystems = [ "zfs" ];
  services.zfs.trim.enable = true;
  services.zfs.autoScrub.enable = true;
  services.zfs.autoSnapshot.enable = true; # enabled on all but /nix

  # unfortunately, I live here
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  time.timeZone = "America/Indiana/Indianapolis";

  nixpkgs.overlays = import ./packages;
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
    mesa
  ];

  # beep boop? boop beep.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # firm ware up daet
  services.fwupd.enable = true;

  # swap caps lock to dual esc+ctrl (!!)
  services.interception-tools.enable = true;

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
  home-manager.users.hazel = import ./home;

  system.stateVersion = "20.03";
}
