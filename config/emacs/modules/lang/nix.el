;;; nix.el --- -*- lexical-binding: t; -*-

;;; Commentary:
;; :snowflake:

;;; Code:
(require 'use-package)
(require 'core/keybindings)

(use-package nix-mode
  :straight t
  :mode "\\.nix\\'"
  :general
  (mode-leader-definer
    :keymaps 'nix-mode-map
    "b" '(nix-build :wk "build")
    "p" '(nix-format-buffer :wk "format")
    "r" '(nix-repl-show :wk "repl")))

(provide 'lang/nix)
;;; nix.el ends here
