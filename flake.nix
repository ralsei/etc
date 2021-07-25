# flake.nix: the glue, but even gluier
{
  description = "maybe you'd have less problems if you weren't on a zoomer distro";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    utils.url = "github:gytis-ivaskevicius/flake-utils-plus";

    home-manager = {
      url = "github:nix-community/home-manager/release-21.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    nixos-hardware.url = "github:nixos/nixos-hardware";

    # temporary
    simple-nixos-mailserver.url = "gitlab:simple-nixos-mailserver/nixos-mailserver/nixos-21.05";

    zsh-syntax-highlighting = {
      url = "github:zsh-users/zsh-syntax-highlighting";
      flake = false;
    };
    perihelion = {
      url = "git+https://git.knightsofthelambdacalcul.us/hazel/perihelion?ref=canon";
      flake = false;
    };
    ziodyne-blog = {
      url = "git+https://git.knightsofthelambdacalcul.us/hazel/blog?ref=canon";
      flake = false;
    };
  };

  outputs = inputs@{ self,
                     utils,
                     nixpkgs,
                     nixpkgs-unstable,
                     nixos-hardware,
                     home-manager,
                     agenix,
                     simple-nixos-mailserver, ... }:
    utils.lib.systemFlake {
      inherit self inputs;

      supportedSystems = [ "x86_64-linux" ];

      channels.unstable.input = nixpkgs-unstable;
      channels.nixpkgs = {
        input = nixpkgs;

        # overrides
        overlaysBuilder = 
        channels: [
          (final: prev: {
            inherit (channels.unstable)
              element-desktop sage rust-analyzer julia-mono;

            hazel = {
              perihelion = prev.callPackage (import inputs.perihelion) {};
              ziodyne-blog = import inputs.ziodyne-blog;
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
        ];
        kerria.modules = [
          ./configuration.nix
          ./machines/kerria
          nixos-hardware.nixosModules.pcengines-apu

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

      sharedModules = [
        nixpkgs.nixosModules.notDetected # enable nonfree firmwares
        home-manager.nixosModules.home-manager
        agenix.nixosModules.age
      ];
    };
}