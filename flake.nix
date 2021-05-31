# flake.nix: the glue, but even gluier
{
  description = "maybe you'd have less problems if you weren't on a zoomer distro";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-20.09";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    utils.url = "github:gytis-ivaskevicius/flake-utils-plus";

    home-manager = {
      url = "github:nix-community/home-manager/release-20.09";
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
  };

  outputs = inputs@{ self,
                     utils,
                     nixpkgs,
                     nixpkgs-unstable,
                     nixos-hardware,
                     home-manager,
                     simple-nixos-mailserver, ... }:
    utils.lib.systemFlake {
      inherit self inputs;

      supportedSystems = [ "x86_64-linux" ];

      channels.unstable.input = nixpkgs-unstable;
      channels.nixpkgs = {
        input = nixpkgs;

        # overrides
        overlaysBuilder = channels: [
          (final: prev: {
            inherit (channels.unstable)
              element-desktop sage rust-analyzer julia-mono;
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
          {
            mailserver = {
              enable = true;
              fqdn = "mail.knightsofthelambdacalcul.us";
              domains = [ "knightsofthelambdacalcul.us" ];

              loginAccounts = {
                "hazel@knightsofthelambdacalcul.us" = {
                  hashedPasswordFile = /etc/snm-hashed-passwd;
                  aliases = [ "postmaster@knightsofthelambdacalcul.us" ];
                };
              };

              certificateScheme = 3; # use letsencrypt

              enableImap = true;
              enableImapSsl = true;
              enablePop3 = false;
              enablePop3Ssl = false;

              enableManageSieve = true;
            };
          }
        ];
      };

      sharedModules = [
        nixpkgs.nixosModules.notDetected # enable nonfree firmwares
        home-manager.nixosModules.home-manager
      ];

      # sharedOverlays = import ./packages;
    };
}
