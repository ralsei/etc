;;; ui/modeline ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; What it says on the tin.

;;; Code:
(require 'use-package)
(require 'ui/theme)

(use-package telephone-line
  :straight t
  :init
  (defface telephone-line-evil-symex
    `((t (:background ,(doom-color 'eg-purple6) :inherit telephone-line-evil)))
    "Face used in evil color-coded segments when in Symex state."
    :group 'telephone-line-evil)

  (telephone-line-defsegment* telephone-line-evil-symex-tag-segment ()
    "Displays current evil mode, or symex mode.
     Configure the face group telephone-line-evil-symex to change the colors per-mode."
    (when (bound-and-true-p evil-mode)
      (let ((tag (cond
                  ((not (evil-visual-state-p)) (upcase (symbol-name evil-state)))
                  ((eq evil-visual-selection 'block)
                   (if telephone-line-evil-use-short-tag "VB" "V-BLOCK"))
                  ((eq evil-visual-selection 'line)
                   (if telephone-line-evil-use-short-tag "VL" "V-LINE"))
                  (t "VISUAL"))))
        (if telephone-line-evil-use-short-tag
            (seq-take tag 2)
          tag))))

  (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
        telephone-line-primary-right-separator 'telephone-line-cubed-right
        telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
        telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right

        telephone-line-height 24)
  :config (telephone-line-mode 1))

(provide 'ui/modeline)
;;; modeline.el ends here
