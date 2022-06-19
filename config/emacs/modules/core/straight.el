;; straight.el -*- lexical-binding: t; -*-
;; Set up the package manager.

;; Load straight.el.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Set up use-package.
(straight-use-package 'use-package)
(require 'use-package)
(setq straight-use-package-by-default t)

(use-package restart-emacs
  :straight t)

(provide 'core/straight)
