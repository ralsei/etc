;;; symexes.el --- -*- lexical-binding: t; -*-

;;; Commentary:
;; Various editing tweaks for symexes (S-expressions), which I work
;; with a lot by merit of being a Racket programmer.

;;; Code:
(require 'use-package)
(require 'core/keybindings)

;; Make paren matching good.
(use-package smartparens
  :straight t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t))

(use-package symex
  :straight t
  :config
  (setq symex--user-evil-keyspec
      '(("j" . symex-go-up)
        ("k" . symex-go-down)
        ("C-j" . symex-climb-branch)
        ("C-k" . symex-descend-branch)
        ("M-j" . symex-goto-highest)
        ("M-k" . symex-goto-lowest)))

  (symex-initialize)

  (global-definer
    "~" '(symex-mode-interface :wk "symex mode")) 
  :custom
  (symex-modal-backend 'evil))

(provide 'editor/symexes)
;;; symexes.el ends here
