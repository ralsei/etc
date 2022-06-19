;;; imenu.el --- -*- lexical-binding: t; -*-

;;; Commentary: Tweak imenu a bit.

;;; Code:
(require 'imenu)
(require 'use-package)
(require 'core/keybindings)

(setq imenu-auto-rescan t)

(use-package imenu-list
  :straight t)

(use-package flimenu
  :straight t
  :config
  (flimenu-global-mode 1))

(global-definer
  "i" '(imenu :wk "imenu"))

(provide 'editor/imenu)
;;; imenu.el ends here
