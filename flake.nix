# flake.nix -- the glue 2: even gluier
{
  description = "maybe you wouldn't have so many problems if you weren't on a zoomer distro";
  # -- paraphrased from sam-tobin hochstadt, 2021

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-20.09";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    # nixpkgs-master.url = "github:nixos/nixpkgs";
    utils.url = "github:gytis-ivaskevicius/flake-utils-plus";

    home-manager = {
      url = "github:nix-community/home-manager/release-20.09";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay/master";
    nixos-hardware.url = "github:nixos/nixos-hardware";
  };

  outputs = inputs@{ self,
                     nixpkgs,
                     nixpkgs-unstable,
                     emacs-overlay,
                     home-manager,
                     nixos-hardware,
                     utils }:
  utils.lib.systemFlake {
    inherit self inputs;

    supportedSystems = [ "x86_64-linux" ]; # I tried running this on aarch64, it didn't end well

    modules = utils.lib.modulesFromList [ ./configuration.nix ]; # fuck it

    channels.nixpkgs = {
      input = nixpkgs;
      overlays = import ./packages;
    };

    # hosts = {
    #   hyacinth.modules = [ ./machines/hyacinth ];
    #   kerria.modules = [ ./machines/kerria ];
    #   lucy.modules = [ ./machines/lucy ];
    # };

    sharedModules = with self.modules; [
      home-manager.nixosModules.home-manager
      utils.nixosModules.saneFlakeDefaults
    ];
  };
}
