# flake.nix: the glue, but even gluier
{
  description = "they won't even let me master the [god-killing fist]?";

  inputs = {
    utils.url = "github:gytis-ivaskevicius/flake-utils-plus";

    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    home-manager = {
      url = "github:nix-community/home-manager/release-23.11";
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

    zsh-syntax-highlighting = {
      url = "github:zsh-users/zsh-syntax-highlighting";
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
                     agenix, ... }:
    utils.lib.mkFlake {
      inherit self inputs;

      supportedSystems = [ "x86_64-linux" ];

      channels.unstable.input = nixpkgs-unstable;
      channels.nixpkgs = {
        input = nixpkgs;

        # overrides
        overlaysBuilder = 
        channels: [
          self.inputs.nix-alien.overlays.default 
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
      };
    };
}
