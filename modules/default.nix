{ options, config, lib, pkgs, ... }:
{
  imports = [
    ./desktop
    ./lang
    ./services
    ./shell
    ./system
    ./tools

    ./agenix.nix
  ];

  options = {
    hazel.home = lib.mkOption {
      type = options.home-manager.users.type.functor.wrapped;
    };
  };

  config = {
    hazel.home = {
      # let home-manager manage itself
      programs.home-manager.enable = true;

      # graphical applications go in desktop/default.nix
      home.packages = with pkgs; [
        neofetch
        pass-wayland
        age
        minisign
        gnupg
        ripgrep
        mosh
        pb_cli
        bitwarden-cli
        tokei
        nix-index
        rmapi
        qmk
      ];

      home.stateVersion = "20.09";
    };
  };
}
