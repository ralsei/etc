{ config, lib, pkgs, ... }:
{
  hazel.home.programs.tmux = {
    enable = true;

    terminal = "screen-256color";

    shortcut = "a";
    keyMode = "vi";

    plugins = with pkgs; [
      tmuxPlugins.gruvbox
      tmuxPlugins.open
      tmuxPlugins.pain-control
    ];

    # vimish
    extraConfig = ''
      bind -n 'M-:'  command-prompt
      bind -n 'M-y' copy-mode
      bind -n 'M-p' run "tmux set-buffer \"$(wl-paste)\"; tmux paste-buffer"

      bind -T copy-mode-vi 'y'   send -X copy-selection-and-cancel
      bind -T copy-mode-vi 'v'   send -X begin-selection
      bind -T copy-mode-vi 'V'   send -X select-line
      bind -T copy-mode-vi 'C-v' send -X rectangle-toggle
      bind -T copy-mode-vi 'i'   send -X cancel
      bind -T copy-mode-vi 'a'   send -X cancel
      bind -T copy-mode-vi '['   send -X start-of-line\; send -X search-backward "❯"
      bind -T copy-mode-vi ']'   send -X end-of-line\;   send -X search-forward  "❯"
    '';
  };
}
