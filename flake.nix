# flake.nix: the glue, but even gluier
{
  description = "maybe you'd have less problems if you weren't on a zoomer distro";

  inputs = {
    utils.url = "github:gytis-ivaskevicius/flake-utils-plus";

    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    home-manager = {
      url = "github:nix-community/home-manager/release-22.11";
      inputs.nixpkgs.follows = "nixpkgs";
    }; 
    
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-alien = {
      url = "github:thiagokokada/nix-alien";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    simple-nixos-mailserver.url = "gitlab:simple-nixos-mailserver/nixos-mailserver/nixos-21.11";

    zsh-syntax-highlighting = {
      url = "github:zsh-users/zsh-syntax-highlighting";
      flake = false;
    };
    perihelion = {
      url = "git+https://git.bicompact.space/hazel/perihelion?ref=canon";
      flake = false;
    };
  };

  outputs = inputs@{ self,
                     utils,
                     nixpkgs,
                     nixpkgs-unstable,
                     nix-alien,
                     nixos-hardware,
                     home-manager,
                     agenix,
                     simple-nixos-mailserver, ... }:
    utils.lib.mkFlake {
      inherit self inputs;

      supportedSystems = [ "x86_64-linux" ];

      channels.unstable.input = nixpkgs-unstable;
      channels.nixpkgs = {
        input = nixpkgs;

        # overrides
        overlaysBuilder = 
        channels: [
          self.inputs.nix-alien.overlay

          (final: prev: {
            hazel = {
              perihelion = prev.callPackage (import inputs.perihelion) {};
            };
          })
        ];
      };
      channelsConfig = { allowUnfree = true; };

      hosts = {
        hyacinth.modules = [
          ./configuration.nix
          ./machines/hyacinth
          nixos-hardware.nixosModules.lenovo-thinkpad-t495

          agenix.nixosModules.age
          home-manager.nixosModules.home-manager
          nixpkgs.nixosModules.notDetected # enable nonfree firmwares
        ];
        kerria.modules = [
          ./configuration.nix
          ./machines/kerria
          nixos-hardware.nixosModules.pcengines-apu

          agenix.nixosModules.age
          home-manager.nixosModules.home-manager
          nixpkgs.nixosModules.notDetected # enable nonfree firmwares


          simple-nixos-mailserver.nixosModule
          ./modules/services/mail.nix

          # FIXME: put this in a better place, lol
          {
            users.users.lemniscation = {
              isNormalUser = true;
              uid = 1001;
              shell = nixpkgs.legacyPackages.x86_64-linux.bash;
            };

            home-manager.users.lemniscation = { pkgs, ... }: {
              home.packages = with pkgs; [ bundler ];
              programs.bash.enable = true;
              programs.zsh.enable = true;

              home.stateVersion = "20.09"; # flake's
            };
          }
        ];
      };
    };
}
