# configuration.nix -- the glue
{ options, config, lib, pkgs, ... }:
{
  imports = [
    <home-manager/nixos>

    ./modules
    ./machines/current
  ];

  nix = {
    autoOptimiseStore = true;
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 10d";
    };
    trustedUsers = [ "root" "hazel" ];

    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
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
    cachix
    cached-nix-shell
  ];

  # unfortunately for everyone, it's me
  users.mutableUsers = false;
  users.users.hazel = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" "audio" "video" "networkmanager" ];
    shell = pkgs.zsh;
  };

  # enable home-manager for my user
  home-manager.users.hazel = lib.mkAliasDefinitions options.hazel.home;

  system.stateVersion = "20.03";
}
