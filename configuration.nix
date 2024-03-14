# configuration.nix -- the glue
{ options, config, lib, pkgs, ... }:
{
  imports = [
    ./modules
  ];

  nix = {
    settings = {
      auto-optimise-store = true;
      substituters = [
        "https://cachix.cachix.org"
        "https://nix-community.cachix.org"
      ];
      trusted-public-keys = [
        "cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
      trusted-users = [ "root" "hazel" ];
    };

    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 10d";
    };

    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
      gc-keep-outputs = true
    '';
  };

  # unfortunately, I live here
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  time.timeZone = "America/Indiana/Indianapolis";
  
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
    # nix-alien
    nix-index
    nix-index-update
    fup-repl
  ];

  programs.nix-ld.enable = true; # the thing that makes it all bearable

  users.mutableUsers = true;

  programs.zsh.enable = true;

  users.users.hazel = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" "audio" "video" "networkmanager" ];
    shell = pkgs.zsh;
  };

  # enable home-manager for my user
  home-manager.users.hazel = lib.mkAliasDefinitions options.my.home;

  system.stateVersion = "20.03";
}
