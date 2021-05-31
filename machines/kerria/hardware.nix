# kerria -- pcengines apu2e0
{ config, lib, pkgs, ... }:
{
  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "ehci_pci" "usb_storage" "sd_mod" "sdhci_pci" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/368840fc-02f0-41be-8756-b216a0423ca0";
      fsType = "ext4";
    };

  fileSystems."/mnt/nextcloud" =
    { device = "/dev/disk/by-uuid/681b0ebd-cd75-4c0d-b22f-9b091be719e0";
      fsType = "ext4";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/5edbc64b-53da-4071-b2ae-ecdfd228550a"; }
    ];

  nix.maxJobs = lib.mkDefault 4;
}
