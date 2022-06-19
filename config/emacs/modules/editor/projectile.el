;;; projectile.el --- -*- lexical-binding: t; -*-

;;; Commentary:
;; Project management.

;;; Code:
(require 'use-package)
(require 'core/keybindings)

(use-package projectile
  :straight t
  :custom (projectile-completion-systeem 'default)
  :functions projectile-add-known-project
  :config
  (projectile-mode 1)
  :general
  (global-definer
    "p" '(:keymap projectile-command-map :package projectile :wk "project")))

(use-package rg
  :straight t
  :config
  (rg-menu-transient-insert "Manage" "e" "Edit" 'wgrep-change-to-wgrep-mode)
  :general
  (global-definer
    "r" '(rg-menu :wk "rg")))

(provide 'editor/projectile)
;;; projectile.el ends here
