;;; lsp.el --- -*- lexical-binding: t; -*-

;;; Commentary: LSP servers. They're good.

;;; Code:
(require 'use-package)
(require 'core/keybindings)

(use-package lsp-mode
  :straight t
  :commands lsp
  :init
  (setq read-process-output-max (* 1024 16))
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-lens-enable nil)
  :config

  (defun tulips/kill-lsp-log-buffers ()
    "Kill all buffers but the current one.
Don't mess with special buffers."
    (interactive)
    (dolist (buffer (buffer-list))
      (when (s-match "lsp-log" (buffer-name buffer))
	(kill-buffer buffer))))

  ;; Ensure that the `*lsp-help*` buffer appears at the bottom of the frame.
  (add-to-list 'display-buffer-alist
	       '("\\*lsp-help\\*"
		 (display-buffer-below-selected display-buffer-at-bottom)
		 (inhibit-same-window . t)
		 (window-height . 15)))
  :general
  (mode-leader-definer
    :keymaps 'lsp-mode-map
    "a" '(lsp-execute-code-action :wk "code action"))
  (global-motion-definer
    :keymaps 'lsp-mode-map
    "a" '(lsp-execute-code-action :wk "code action")
    "d" '(lsp-find-definition :wk "goto definition")
    "i" '(lsp-find-implementation :wk "goto implementation")
    "r" '(lsp-find-references :wk "find references")
    "D" '(xref-pop-marker-stack :wk "go back"))
  (general-define-key
   :states 'normal
   :keymaps 'lsp-mode-map
   "K" 'lsp-describe-thing-at-point))

(provide 'editor/lsp)
;;; lsp.el ends here
