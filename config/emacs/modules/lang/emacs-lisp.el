;;; emacs-lisp.el --- -*- lexical-binding: t; -*-

;;; Commentary:
;; It's like Common Lisp, but worse!

;;; Code:
(require 'use-package)
(require 'core/keybindings)
(require 'editor/snippets)

(create-file-template ".*.el$" "emacs-lisp-template" 'emacs-lisp-mode)

(setq-default flycheck-emacs-lisp-load-path 'inherit)

(mode-leader-definer
  :keymaps 'emacs-lisp-mode-map
  "e" '(eval-last-sexp :wk "eval sexp")
  "E" '(eval-print-last-sexp :wk "print sexp")
  "l" '(eval-buffer :wk "load buffer")
  "L" '(emacs-lisp-native-compile-and-load :wk "load and compile buffer")
  "s" '(emacs-lisp-switch-to-repl :wk "switch to repl"))

(provide 'lang/emacs-lisp)
;;; emacs-lisp.el ends here
