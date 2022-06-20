;;; tools/pdf ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; I like PDF-Tools it's good

;;; Code:
(require 'use-package)
(require 'core/keybindings)

(use-package pdf-tools
  :straight t
  :mode ("\\.pdf\\'" . pdf-tools-mode)
  :config
  (evil-collection-pdf-setup)

  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-view-use-scaling t
	pdf-view-use-imagemagick t))

(use-package saveplace-pdf-view
  :straight t
  :after pdf-view)

(provide 'tools/pdf)
;;; pdf.el ends here
