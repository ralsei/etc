;;; ui/modeline ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; I'm too used to DOOM, so I copy its modeline.

;;; Code:
(require 'use-package)

(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
  :hook (doom-modeline-mode . size-indication-mode)
  :hook (doom-modeline-mode . column-number-mode)
  :config
  (setq doom-modeline-support-imenu t
	doom-modeline-buffer-file-name-style 'relative-from-project))

(provide 'ui/modeline)
;;; modeline.el ends here
