;;; lang/sage ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; The only good calculator

;;; Code:
(require 'use-package)

(use-package sage-shell-mode
  :straight t
  :config
  (sage-shell:define-alias))

(provide 'lang/sage)
;;; sage.el ends here
