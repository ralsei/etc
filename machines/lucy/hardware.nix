# lucy -- lenovo thinkpad t440
{ config, lib, pkgs, ... }:
{
  imports =
    [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ehci_pci" "ahci" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
  boot.initrd.kernelModules = [ "dm-snapshot" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/548331a9-9060-4042-ac42-415f8c6e12ad";
      fsType = "xfs";
    };

  fileSystems."/home" =
    { device = "/dev/disk/by-uuid/63868b0b-b4fb-4066-86a8-fbf7c5f92434";
      fsType = "xfs";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/8FCC-78AE";
      fsType = "vfat";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/227f922f-4f62-4f48-a528-264b07205f2a"; }
    ];

  # encryption
  boot.initrd.luks.devices.pool = {
    device = "/dev/disk/by-uuid/262c7568-181d-447c-b693-2abfd2dd16c9";
    preLVM = true;
    allowDiscards = true;
  };

  nix.maxJobs = lib.mkDefault 4;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
