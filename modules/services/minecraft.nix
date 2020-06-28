{ config, lib, pkgs, ... }:
let
  cfg = config.hazel.services.minecraft;
in
with lib; {
  options = {
    hazel.services.minecraft = {
      enable = mkEnableOption "minecraft server";
    };
  };

  config = mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = [ 25565 25575 ]; # MC and RCON
    networking.firewall.allowedUDPPorts = [ 25565 25575 ];

    services.minecraft-server = {
      enable = true;
      eula = true;
      declarative = true;
      jvmOpts = "-Xms1536M -Xmx1536M"; # 2GB Raspberry Pi 4

      serverProperties = {
        motd = "too gay for quay";
        max-players = 8;
        white-list = true;
        gamemode = 0;
        enable-rcon = true;
        "rcon.password" = (builtins.readFile /etc/mc-rcon-password);
      };

      # if you want to be here, email me
      whitelist = {
        hazelisdumb = "f43bd8ef-c8d2-4457-bf94-d07ad1dcb89d";
        MadelineRose = "93085a13-b1fd-40fb-8e86-b32970d0914e";
        132ikl = "bc71a7e3-cfbb-48c3-ae8f-887eb2abda52";
      };
    };
  };
}
