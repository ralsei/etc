;;; cooltt.el --- -*- lexical-binding: t; -*-

;;; Commentary:
;; Support the cooltt proof assistant.

;;; Code:
(require 'use-package)
(require 'core/keybindings)

(use-package cooltt
  :straight (:type git
	     :host github
	     :repo "RedPRL/cooltt"
	     :files ("emacs/cooltt.el"))
  :config
  (setq cooltt-command "dune exec cooltt --")
  (add-hook 'cooltt-mode-hook #'(lambda () (activate-input-method "TeX")))
  :general
  (mode-leader-definer
    :keymaps 'cooltt-mode-map
    "l" '(cooltt-compile-buffer :wk "load")))

(provide 'lang/cooltt)
;;; cooltt.el ends here
