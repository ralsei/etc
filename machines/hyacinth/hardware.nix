# hyacinth -- lenovo thinkpad t495
{ config, lib, pkgs, ... }:

{
  boot.initrd.availableKernelModules = [ "nvme" "ehci_pci" "xhci_pci" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-amd" "cp210x" "v4l2loopback" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "rpool/root/nixos";
      fsType = "zfs";
    };

  fileSystems."/nix" =
    { device = "rpool/root/nix";
      fsType = "zfs";
    };

  fileSystems."/home" =
    { device = "rpool/home";
      fsType = "zfs";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/CCF8-E5E8";
      fsType = "vfat";
    };

  swapDevices = [ ];

  nix.settings.max-jobs = lib.mkDefault 8;
}
