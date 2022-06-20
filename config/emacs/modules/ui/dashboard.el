;;; ui/dashboard ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; A decent startup screen.

;;; Code:
(require 'use-package)
(require 'core/keybindings)

(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  (evil-collection-dashboard-setup)

  (setq initial-buffer-choice
	(lambda ()
	  (get-buffer-create "*dashboard*"))))

(provide 'ui/dashboard)
;;; dashboard.el ends here
