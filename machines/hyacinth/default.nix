# hyacinth -- system-specific settings
{ config, pkgs, lib, ... }:
{
  imports = [
    ./hardware.nix
  ];

  networking.hostName = "hyacinth";
  networking.hostId = "3ae0d799";

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.tmp.cleanOnBoot = true;

  boot.supportedFilesystems = [ "zfs" ];
  services.zfs.trim.enable = true;
  services.zfs.autoScrub.enable = true;
  services.zfs.autoSnapshot.enable = false;
  # auto-snapshots are more trouble than it's worth

  networking.useDHCP = false;
  networking.interfaces.enp3s0f0.useDHCP = true;
  networking.interfaces.enp4s0.useDHCP = true;
  networking.interfaces.wlp1s0.useDHCP = true;

  sound.enable = true;
  my.pipewire.enable = true;
  hardware.bluetooth.enable = true;

  my.networking.wifi = true;

  my.autoMount.enable = true;

  my.yubikey = {
    enable = true;
    login = true;
  };

  my.graphicalSession.enable = true;

  my.emacs = {
    enable = true;
    daemon = false; # eh
  };
  my.langSupport.enable = true;
  my.services.mpd.enable = true;

  my.laptopPower.enable = true;

  services.fwupd.enable = true;

  services.interception-tools = {
    enable = true;

    plugins = [ pkgs.interception-tools-plugins.caps2esc ];

    # sudo uinput -p -d /dev/input/event0
    udevmonConfig = ''
      - JOB: "${pkgs.interception-tools}/bin/intercept -g $DEVNODE | ${pkgs.interception-tools-plugins.caps2esc}/bin/caps2esc | ${pkgs.interception-tools}/bin/uinput -d $DEVNODE"
        DEVICE:
          NAME: AT Translated Set 2 keyboard
          EVENTS:
            EV_KEY: [KEY_CAPSLOCK, KEY_ESC]
    '';
  };

  hardware.opengl.enable = true;
  hardware.opengl.driSupport32Bit = true;
}
