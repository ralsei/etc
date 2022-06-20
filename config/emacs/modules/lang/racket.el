;;; lang/racket --- -*- lexical-binding: t; -*-

;;; Commentary:
;; ((λ (λ) (λ λ)) (λ (λ) (λ λ)))

;;; Code:
(require 'use-package)
(require 'core/keybindings)
(require 'editor/snippets)
(require 'editor/projectile)

(with-eval-after-load "projectile"
  (add-to-list 'projectile-project-root-files "info.rkt"))

(use-package racket-mode
  :straight t
  :mode "\\.rkt\\'"
  :config
  (add-hook 'racket-mode-local-vars-hook #'racket-xp-mode)
  :general
  (mode-leader-definer
    :keymaps 'racket-mode-map
    "r" '(racket-run :wk "run")
    "R" '(racket-run-and-switch-to-repl :wk "run → REPL")
    "t" '(racket-test :wk "test")
    "y" '(racket-insert-lambda :wk "insert λ")))

(create-file-template ".*.rkt$" "racket-template" 'racket-mode)

(use-package scribble-mode
  :straight (:host github :repo "emacs-pe/scribble-mode"))

(provide 'lang/racket)
;;; racket.el ends here
