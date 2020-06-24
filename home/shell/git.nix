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
     
      init.templateDir = "~/.config/git/template";
    };
  };

  # HOO BOY
  # I have used most of these at least once
  programs.zsh.shellAliases = {
    "g" = "git";

    # branching
    "gb" = "git branch";
    "gbc" = "git checkout -b";
    "gbd" = "git branch --delete";
    "gbD" = "git branch --delete --force";
    "gco" = "git checkout";
    "gm" = "git merge";

    # committing
    "gc" = "git commit --verbose";
    "gca" = "git commit --amend --reuse-message HEAD";
    "gcm" = "git commit --message";

    # fetching and pulling
    "gcl" = "git clone";
    "gcL" = "git clone --recurse-submodules";
    "gfm" = "git pull";

    # indexing, adding
    "gia" = "git add";
    "giA" = "git add -A";
    "gir" = "git reset";

    # log, status, diff
    "gl" = "git log --topo-order";
    "gls" = "git log --topo-order --stat";
    "gld" = "git log --topo-order --stat --patch --full-diff";
    "glc" = "git shortlog --summary --numbered";
    "glS" = "git log --show-signature";
    "gd" = "git diff --no-ext-diff";
    "gD" = "git diff --no-ext-diff --word-diff";
    "gs" = "git status --short";
    "gS" = "git status";

    # push
    "gp" = "git push";
    "gpF" = "git push --force-with-lease";

    # rebase
    "gr" = "git rebase";
    "gra" = "git rebase --abort";
    "grc" = "git rebase --continue";
    "gri" = "git rebase --interactive";
    "grs" = "git rebase --skip";

    # remote
    "gR" = "git remote";
    "gRa" = "git remote add";
    "gRx" = "git remote rm";
    "gRm" = "git remote rename";
    "gRs" = "git remote set-url";
    "gRu" = "git remote update";
  };

  xdg.configFile."bw-git-helper/config.ini".text = ''
    [*github.com*]
    target=7734c9e1-8174-4796-846b-feefe5f88d2c
   
    [*knightsofthelambdacalcul.us*]
    target=a268d28a-12f2-487e-8333-c3ddbd834e76

    [config]
    pinentry=pinentry-gtk-2
  '';
  
  home.packages = with pkgs; [ hazel.bw-git-helper ];
}
