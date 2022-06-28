;;; ui/fonts ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; Set up fonts, with reasonable fallbacks.

;;; Code:
(require 'use-package)

;; XFT font strings seem to work better in my experience.
(defun tulips/xft-font-string (name size &optional properties)
  "Generate an XFT font string from the font NAME and the size SIZE."
  (concat (format "%s-%d" name size)
          (when properties
            (apply #'concat (mapcar (lambda (prop)
                                      (format ":%s=%s" (car prop) (cdr prop)))
                                    properties)))))

(defvar tulips/monospace-font
  (tulips/xft-font-string "Pragmata Pro" 11
                       '((hintstyle . 3)
                         (hinting . true)
                         (lcdfilter . 3)
                         (antialias . true))))
;; This is a relic from my DOOM config. I don't actually know how to set it.
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

;(set-face-attribute 'default nil :font tulips/monospace-font)

;; Try and get Unicode fallback fonts.
;(set-fontset-font "fontset-default" nil tulips/monospace-font)
;(set-fontset-font "fontset-default" nil tulips/unicode-font nil 'append)

;; Load the Unicode Fonts package as a backup, just in case.
;; If we get here, the stuff displayed is ugly.
;(use-package unicode-fonts
;  :straight t
;  :config (unicode-fonts-setup))

;; Use all-the-icons.
(use-package all-the-icons
  :straight t)

(provide 'ui/fonts)
;;; fonts.el ends here
