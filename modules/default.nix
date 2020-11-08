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
        neofetch
        pass-wayland
        age
        minisign
        gnupg
        ripgrep
        mosh
        pb_cli
      ] ++ (if builtins.currentSystem != "aarch64-linux" then [
        # these apps take forever to compile
        bitwarden-cli
        tokei
        nix-index
      ] else []);

      home.stateVersion = "20.03";
    };
  };
}
