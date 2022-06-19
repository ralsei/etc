;;; lang/rust ---  -*- lexical-binding: t; -*-

;;; Commentary: >::<Vec<u32>>

;;; Code:
(require 'use-package)
(require 'editor/lsp)

(use-package rust-mode
  :straight t
  :config
  (add-hook 'rust-mode-hook #'lsp)
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq lsp-rust-analyzer-diagnostics-disabled (vector "mismatched-arg-count")))

(provide 'lang/rust)
;;; rust.el ends here
