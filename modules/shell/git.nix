{ config, lib, pkgs, ... }:
{
  my.home = {
    programs.git = {
      enable = true;
      package = pkgs.gitAndTools.gitFull;

      userName = "tulip amalie";
      userEmail = "tulip@bicompact.space";
      signing = {
        key = "593067D35E5CA280959CEC3735C1205716621182";
        signByDefault = false;
      };

      aliases = {
        wip = ''!git add --all && git commit --message "WIP - $(date)"'';
      };

      delta.enable = true;
    };

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
  };
}
