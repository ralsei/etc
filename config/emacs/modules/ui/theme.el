;;; ui/theme ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; Load up my current theme, whatever that may be.

;;; Code:
(require 'use-package)

(use-package doom-themes
  ; :straight (:host github
  ;            :repo "JuneKelly/emacs-doom-themes"
  ;            :branch "jk-earl-grey-theme-add-berry")
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-acario-dark t)

  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(provide 'ui/theme)
;;; theme.el ends here
