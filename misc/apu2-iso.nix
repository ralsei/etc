# this is a minimal environment for installing NixOS on the PC Engines apu2,
# notably with serial and SSH capabilities.
{ config, lib, pkgs, ... }:
{
  imports = [
    <nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix>
    <nixpkgs/nixos/modules/installer/cd-dvd/channel.nix>
  ];

  systemd.services.sshd.wantedBy = pkgs.lib.mkForce [ "multi-user.target" ];
  users.users.root.openssh.authorizedKeys.keys = [
    (builtins.readFile /home/hazel/.ssh/id_rsa.pub)
  ];

  boot.loader.grub.extraConfig = ''
    serial --speed=115200 --unit=0 --word=8 --parity=no --stop=1
    terminal_input serial
    terminal_output serial
  '';
  boot.kernelPackages = pkgs.linuxPackages_4_19;
  boot.kernelParams = [ "console=ttyS0,115200n8" ];
}
