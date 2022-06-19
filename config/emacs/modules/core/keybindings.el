;; keybindings.el -*- lexical-binding: t; -*-
;; Set up DOOM-ish keybindings.
(require 'use-package)

;; Use general.el for bindings.
(use-package general
  :straight t
  :functions
  global-definer
  global-motion-definer
  model-leader-definer
  :config
  (general-evil-setup)
  (general-auto-unbind-keys))

(require 'general)

;; Set up the spacebar as a global leader key.
(general-create-definer global-definer
  :keymaps 'override
  :states '(insert emacs normal hybrid motion visual operator)
  :prefix "SPC"
  :non-normal-prefix "C-SPC")

(global-definer "SPC" '(execute-extended-command :wk "execute command"))

;; Create a "general" definer for vim motions in normal/visual/motion mode, which are prefixed
;; with 'g'.
(general-create-definer global-motion-definer
  :keymaps 'override
  :states '(normal motion visual operator)
  :prefix "g")

(general-create-definer mode-leader-definer
  :states '(normal motion)
  :wrapping global-definer
  :prefix "SPC m"
  "" '(:ignore t :which-key "mode"))

(use-package evil
  :straight t
  :init
  (setq evil-esc-delay 0
        evil-want-keybinding nil
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-respect-visual-line-mode t
        evil-undo-system 'undo-redo) ; Only Emacs 28+!
  (add-hook 'with-editor-mode-hook 'evil-insert-state)
  :config
  (evil-mode 1))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :straight t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-nerd-commenter
  :straight t
  :config
  :general
  (global-motion-definer
    "c" 'evilnc-comment-operator))

(use-package which-key
  :straight t
  :diminish which-key-mode
  :config (which-key-mode))

(provide 'core/keybindings)
