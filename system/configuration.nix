{ config, pkgs, lib, ... }:
{
  imports =
    [
      ./hardware-configuration.nix
      ./desktop.nix
      ./fonts.nix
      ./networking.nix
      ./power.nix
    ];

  # systemd-boot
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # clear /tmp on reboot
  boot.cleanTmpDir = true;

  # enable ZFS support
  boot.supportedFilesystems = [ "zfs" ];
  services.zfs.trim.enable = true;

  # unfortunately, I live here
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  time.timeZone = "America/Indiana/Indianapolis";

  nixpkgs.config.allowUnfree = true; # sorry, Stallman

  # the bare minimum
  environment.systemPackages = with pkgs; [
    coreutils
    git
    killall
    unzip
    wget 
    vim
    cryptsetup
    gnumake
    ripgrep
    mesa
  ];

  # beep boop? boop beep.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # the true keyboard layout
  services.interception-tools.enable = true;

  # firm ware up daet
  services.fwupd.enable = true;

  # unfortunately for everyone, it's me
  users.users.hazel = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" "audio" "video" "networkmanager" ];
    shell = pkgs.zsh;
  };

  system.stateVersion = "20.03";
}

