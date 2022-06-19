;;; ui.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; Theme everything.

;;; Code:
(require 'use-package)

;; Set up fonts.
(defun tulips/xft-font-string (name size &optional properties)
  "Generate an XFT font string from the font NAME and the size SIZE."
  (concat (format "%s-%d" name size)
          (when properties
            (apply #'concat (mapcar (lambda (prop)
                                      (format ":%s=%s" (car prop) (cdr prop)))
                                    properties)))))

(defvar tulips/monospace-font
  (tulips/xft-font-string "Source Code Pro" 10
                       '((hintstyle . 3)
                         (hinting . true)
                         (lcdfilter . 3)
                         (antialias . true))))
(defvar tulips/variable-pitch-font
  (tulips/xft-font-string "IBM Plex Sans" 10
                       '((hintstyle . 3)
                         (autohint . true)
                         (lcdfilter . 3)
                         (antialias . true))))
(defvar tulips/unicode-font
  (tulips/xft-font-string "DejaVu Sans Mono" 10
                       '((hintstyle . 3)
                         (autohint . true)
                         (lcdfilter . 3)
                         (antialias . true))))

(set-face-attribute 'default nil :font tulips/monospace-font)
(set-fontset-font "fontset-default" nil tulips/monospace-font)
(set-fontset-font "fontset-default" nil tulips/unicode-font nil 'append)

;; Load the Unicode Fonts package as a backup, just in case.
(use-package unicode-fonts
  :straight t
  :config (unicode-fonts-setup))

;; Use all-the-icons.
(use-package all-the-icons
  :straight t)

;; Set up the Earl Grey theme.
;; This uses a fork of DOOM's themes.
(use-package doom-themes
  :straight (:host github
             :repo "JuneKelly/emacs-doom-themes"
             :branch "jk-earl-grey-theme-add-berry")
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-earl-grey t)

  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; Use DOOM's modeline.
;; I'm too used to DOOM, huh?
(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1))

;; Line numbers.
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(provide 'core/ui)
;;; ui.el ends here
