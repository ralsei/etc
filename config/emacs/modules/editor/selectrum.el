;;; selectrum.el --- -*- lexical-binding: t; -*-

;;; Commentary: Actually decent fuzzy finding.

;;; Code:
(require 'use-package)

(require 'core/keybindings)

(use-package selectrum
  :straight t
  :config
  (selectrum-mode 1))

(use-package selectrum-prescient
  :straight t
  :after selectrum
  :config
  (selectrum-prescient-mode 1))

(use-package ctrlf
  :straight t
  :config
  (ctrlf-change-search-style 'fuzzy)
  :general
  (general-def
    :keymaps 'ctrlf-mode-map
    "C-j" #'ctrlf-forward-literal
    "C-k" #'ctrlf-backward-literal)
  (general-def
    :keymaps 'override
    :states '(normal motion visual operator)
    "/" '(ctrlf-forward-literal :wk "search")))

(provide 'editor/selectrum)
;;; selectrum.el ends here
