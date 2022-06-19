;;; direnv.el --- -*- lexical-binding: t; -*-

;;; Commentary:
;; I use direnv pretty heavily because NixOS is like that.

;;; Code:
(require 'use-package)

(use-package direnv
  :straight t
  :config
  (direnv-mode))

(provide 'tools/direnv)
;;; direnv.el ends here
