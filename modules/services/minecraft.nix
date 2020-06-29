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
      package = pkgs.hazel.papermc;

      enable = true;
      eula = true;
      declarative = true;

      # https://aikar.co/2018/07/02/tuning-the-jvm-g1gc-garbage-collector-flags-for-minecraft/
      jvmOpts = "-Xms6000M -Xmx6000M -XX:+UseG1GC -XX:+ParallelRefProcEnabled -XX:MaxGCPauseMillis=200 -XX:+UnlockExperimentalVMOptions -XX:+DisableExplicitGC -XX:+AlwaysPreTouch -XX:G1NewSizePercent=30 -XX:G1MaxNewSizePercent=40 -XX:G1HeapRegionSize=8M -XX:G1ReservePercent=20 -XX:G1HeapWastePercent=5 -XX:G1MixedGCCountTarget=4 -XX:InitiatingHeapOccupancyPercent=15 -XX:G1MixedGCLiveThresholdPercent=90 -XX:G1RSetUpdatingPauseTimePercent=5 -XX:SurvivorRatio=32 -XX:+PerfDisableSharedMem -XX:MaxTenuringThreshold=1 -Dusing.aikars.flags=https://mcflags.emc.gs -Daikars.new.flags=true";

      serverProperties = {
        motd = "too gay for quay";
        max-players = 4;
        white-list = true;
        gamemode = 0;
        enable-rcon = true;
        "rcon.password" = (builtins.readFile /etc/mc-rcon-password);

        view-distance = 6;
        use-faster-eigencraft-redstone = true;
        prevent-moving-into-unloaded-chunks = true;
      };

      # if you want to be here, email me
      whitelist = {
        hazelisdumb = "f43bd8ef-c8d2-4457-bf94-d07ad1dcb89d";
        MadelineRose = "93085a13-b1fd-40fb-8e86-b32970d0914e";
        "132ikl" = "bc71a7e3-cfbb-48c3-ae8f-887eb2abda52";
      };
    };
  };
}
