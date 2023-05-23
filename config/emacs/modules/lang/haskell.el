;;; lang/haskell ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; Yeah

;;; Code:
(require 'core/straight)
(require 'core/keybindings)
(require 'core/sanity)
(require 'editor/lsp)

(use-package haskell-mode
  :straight t
  :mode "\\.hs\\'"
  :init
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
  (add-hook 'haskell-mode-hook #'lsp)
  (add-hook 'haskell-cabal-mode-hook
	    (lambda ()
	      (setq-local tab-width 4)))
  :config
  ;; GHCI Options
  (let ((ghci-options
	 '("-ferror-spans"
	   "-fdefer-typed-holes"
	   "-fno-diagnostics-show-caret"
	   "-fmax-valid-hole-fits=0"
	   "-fbyte-code"
	   "-Wwarn")))
    (setq haskell-process-args-stack-ghci
	  (list (concat "--ghci-options=" (string-join ghci-options " "))
		"--no-build"
		"--no-load"))
    (setq haskell-process-args-ghci ghci-options)
    (setq haskell-process-args-cabal-repl
	  (mapcar (lambda (opt) (concat "--repl-options=" opt)) ghci-options)))

  ;; Interactive Haskell Mode Stuff
  (setq haskell-process-use-presentation-mode t)
  (setq haskell-process-auto-import-loaded-modules t)
  (setq haskell-process-suggest-remove-import-lines nil)
  (setq haskell-ask-also-kill-buffers t)
  (setq haskell-process-show-overlays nil)
  (setq haskell-interactive-popup-errors nil)

  ;; Formatting
  (setq haskell-font-lock-keywords nil)
  (setq haskell-indentation-left-offset 4)
  (setq haskell-indentation-layout-offset 4)

  ;; Helper Functions
  (defun haskell-next-decl-name ()
    "Get the name of the next declaration."
    (save-excursion
      (haskell-ds-forward-decl)
      (haskell-ident-at-point)))

  :general
  (mode-leader-definer
    :keymaps 'haskell-mode-map
    "s" '(haskell-interactive-switch :wk "switch to interactive")
    "i" '(haskell-process-do-info :wk "info")
    "t" '(haskell-mode-show-type-at :wk "type")
    "l" '(haskell-process-load-file :wk "load")
    "r" '(haskell-process-reload :wk "reload")
    "T" '(haskell-session-change-target :wk "change target")
    "R" '(haskell-process-restart :wk "restart process"))

  (global-motion-definer
    :keymaps 'haskell-mode-map
    "k" '(beginning-of-defun :wk "top of definition")
    "j" '(end-of-defun :wk "bottom of definition")
    "d" '(lsp-find-definition :wk "goto definition")
    "r" '(xref-find-references :wk "find references"))

  (global-motion-definer
    :keymaps 'haskell-interactive-mode-map
    "e" '(next-error :wk "goto error"))

  (mode-leader-definer
    :keymaps
    'haskell-interactive-mode-map
    "s" '(haskell-interactive-switch-back :wk "switch to source")))


(use-package lsp-haskell
  :straight t
  :after haskell
  :config
  (setq lsp-haskell-hlint-on nil)

  (defun lsp-haskell-execute-wingman-action (kind)
    "Execute a wingman action of type KIND.  This will prompt the user if there are multiple options."
    (lsp-execute-code-action (lsp--select-action (lsp-code-actions-at-point (concat "refactor.wingman." kind)))))

  (defun lsp-haskell-wingman-introduce-lambda ()
    "Introduce a lambda abstraction when the cursor is over a hole."
    (interactive)
    (lsp-haskell-execute-wingman-action "introduceLambda"))

  (defun lsp-haskell-wingman-fill-hole ()
    "Attempt to fill a hole using wingman."
    (interactive)
    (lsp-haskell-execute-wingman-action "fillHole"))

  (defun lsp-haskell-wingman-case-split ()
    "Perform a case split using wingman."
    (interactive)
    (lsp-haskell-execute-wingman-action "caseSplit"))
  :general
  (mode-leader-definer
    :keymaps 'haskell-mode-map
    "c" '(lsp-haskell-case-split :wk "case split")))

(provide 'lang/haskell)
;;; haskell.el ends here
