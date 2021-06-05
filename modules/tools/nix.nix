{ config, lib, pkgs, ... }:
{
  hazel.home = {
    services.lorri.enable = true;

    programs.direnv = {
      enable = true;
      enableZshIntegration = true;
      stdlib = ''
        use_flake() {
          watch_file flake.nix
          watch_file flake.lock
          eval "$(nix print-dev-env --profile "$(direnv_layout_dir)/flake_profile")"
        }
      '';
    };

    home.packages = with pkgs; [
      niv
    ];
  };
}
