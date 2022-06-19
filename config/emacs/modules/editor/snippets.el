;;; snippets.el --- -*- lexical-binding: t; -*-

;;; Commentary:
;; YASnippet stuff.

;;; Code:
(require 'use-package)
(require 'core/keybindings)

(use-package yasnippet
  :straight t
  :diminish yas-minor-mode
  :init
  (setq yas-indent-line 'fixed)
  :config
  (yas-global-mode 1))

(general-create-definer snippet-menu-definer
  :states '(normal motion)
  :wrapping global-definer
  :prefix "SPC s"
  "" '(:ignore t :wk "snippets"))

(snippet-menu-definer
  "n" '(yas-new-snippet :wk "new snippet")
  "i" '(yas-insert-snippet :wk "insert snippet")
  "e" '(yas-visit-snippet-file :wk "edit snippet"))

;; Automatically load certain snippets on file open.
(use-package autoinsert
  :straight t
  :custom
  (auto-insert-query nil)
  (auto-insert-alist nil)
  :preface
  (defun create-file-template (regex template mode)
    "Automatically insert the TEMPLATE snippet when REGEX match the file name."
    (add-to-list 'auto-insert-alist
		 `(,regex . [(lambda () (yas-expand-snippet (yas-lookup-snippet ,template ',mode)))])))
  :config
  ;; When we open a new file, automatically insert the file template
  (auto-insert-mode 1)
  (add-hook 'find-file-hook 'auto-insert))

(provide 'editor/snippets)
;;; snippets.el ends here
