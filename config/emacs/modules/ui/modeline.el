;;; ui/modeline ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; What it says on the tin.

;;; Code:
(require 'use-package)
(require 'editor/workspaces)
(require 'ui/theme)

;; Nothing flashes.
(setq ring-bell-function 'ignore)

(use-package telephone-line
  :straight t
  :init
  (defface telephone-line-evil-symex
    `((t (:background ,(doom-color 'eg-purple6) :inherit telephone-line-evil)))
    "Face used in evil color-coded segments when in Symex state."
    :group 'telephone-line-evil)

  (telephone-line-defsegment* telephone-line-workspace-segment ()
    "Gets the current workspace (tab) to display in the modeline."
    (let ((current-ws-name (tulips/current-workspace))
          (ntabs (length (tab-bar-tabs))))
      (cond ((= ntabs 1) "")
            (t current-ws-name))))

  (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
        telephone-line-primary-right-separator 'telephone-line-cubed-right
        telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
        telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right

        telephone-line-height 24

        telephone-line-lhs
        '((evil . (telephone-line-evil-tag-segment))
          (accent . (telephone-line-vc-segment
                     telephone-line-process-segment))
          (nil . (telephone-line-projectile-segment
                  telephone-line-buffer-segment)))

        telephone-line-rhs
        '((nil . (telephone-line-flycheck-segment
                  telephone-line-workspace-segment
                  telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment))
          (evil . (telephone-line-airline-position-segment) )))
  :config (telephone-line-mode 1))

(provide 'ui/modeline)
;;; modeline.el ends here
