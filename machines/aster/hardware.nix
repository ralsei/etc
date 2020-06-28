# aster -- raspberry pi 4
{ config, lib, pkgs, ... }:
{
  imports =
    [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ];

  boot.initrd.availableKernelModules = [ ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-label/FIRMWARE";
      fsType = "vfat";
    };

  # if you're here, I fucked up
  swapDevices = [ { device = "/swapfile"; size = 1024; } ];

  nix.maxJobs = lib.mkDefault 4;
  powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";
}
