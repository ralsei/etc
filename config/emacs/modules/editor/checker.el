;;; checker.el --- -*- lexical-binding: t; -*-

;;; Commentary: Syntax checking via Flycheck.

;;; Code:
(require 'use-package)
(require 'core/keybindings)

(use-package flycheck
  :straight t
  :config
  (global-flycheck-mode)
  :commands flycheck-mode)

(general-define-key
 :states '(normal motion)
 :keymaps 'flycheck-error-list-mode-map
 "q" 'quit-window)

(general-create-definer error-menu-definer
  :states '(normal motion)
  :wrapping global-definer
  :prefix "SPC e"
  "" '(:ignore t :wk "error"))

(error-menu-definer
  :keymaps 'flycheck-mode-map
  "e" '(flycheck-list-errors :wk "list errors")
  "j" '(flycheck-next-error :wk "next error")
  "k" '(flycheck-previous-error :wk "prev error"))

(provide 'editor/checker)
;;; checker.el ends here
