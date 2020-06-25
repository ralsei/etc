{ options, config, lib, pkgs, ... }:
{
  imports = [
    ./desktop
    ./lang
    ./services
    ./shell
    ./system
    ./tools
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
        nix-index # for file searchin'
        neofetch
        tokei
        bitwarden-cli
        pass-wayland
        age
        minisign
        gnupg
        ripgrep
        hazel.linx-client
        mosh
      ];

      home.stateVersion = "20.03";
    };
  };
}
