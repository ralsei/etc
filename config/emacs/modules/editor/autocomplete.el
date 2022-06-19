;; autocomplete.el -*- lexical-binding: t; -*-
;; Autocompletion.
(require 'use-package)

(use-package company
  :straight t
  :diminish company-mode
  :commands company-mode
  :config
  (global-company-mode))

(provide 'editor/autocomplete)
