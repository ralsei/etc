;;; init.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; 
;; ( U         U       _  U    \ U
;;  \| )     _ | /      )/      \| /7
;; __|/__   ( \|(__   __(__    __|/__
;; \    /     \   /   \   /    \    /
;;  \__/       \_/     \_/      \__/
;;
;; Tentatively mostly stolen from @TOTBWF.

;;; Code:
;; Not technically correct but okay.
(setq user-full-name "Hazel Levine"
      user-mail-address "hazel@knightsofthelambdacalcul.us")

;; Grab all the modules.
(add-to-list 'load-path "~/.emacs.d/modules")

;; Set up straight.el, the scaffolding for keybindings, and the UI.
(require 'core/straight)
(require 'core/sanity)
(require 'core/keybindings)
(require 'core/ui)

;; Enable serious tweaks to the editing experience, that don't have
;; to be loaded immediately.
(require 'editor/autocomplete)
(require 'editor/checker)
(require 'editor/keybindings)
(require 'editor/lsp)
(require 'editor/projectile)
(require 'editor/selectrum)
(require 'editor/snippets)
(require 'editor/symexes)
(require 'editor/workspaces)

;; Set up various non-language tools.
(require 'tools/direnv)
(require 'tools/magit)

;; Set up the programming languages I write.
(require 'lang/agda)
(require 'lang/cooltt)
(require 'lang/emacs-lisp)
(require 'lang/nix)
(require 'lang/ocaml)
(require 'lang/racket)
(require 'lang/rust)

(provide 'init)
;;; init.el ends here
