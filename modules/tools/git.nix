{ config, lib, pkgs, ... }:
{
  programs.git = {
    enable = true;
    userName = "hazel levine";
    userEmail = "rose.hazel@protonmail.ch";
    extraConfig = {
      credential = {
        helper = "!bw-git-helper $@";
        useHttpPath = true;
      };
    };
  };

  xdg.configFile."bw-git-helper/config.ini".text = ''
    [*github.com*]
    target=7734c9e1-8174-4796-846b-feefe5f88d2c
   
    [*qtp2t.club*]
    target=a268d28a-12f2-487e-8333-c3ddbd834e76

    [config]
    pinentry=pinentry-gtk-2
  '';
  
  home.packages = with pkgs; [
    hazel.bw-git-helper
  ];
}
