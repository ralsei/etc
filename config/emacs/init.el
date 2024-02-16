;;; init ---  -*- lexical-binding: t; -*-

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
(setq user-full-name "Tulip Amalie"
      user-mail-address "hazel@knightsofthelambdacalcul.us")

;; Grab all the modules.
(add-to-list 'load-path "~/.emacs.d/modules")

;; Set up straight.el, the scaffolding for keybindings, and the UI.
(require 'core/straight)
(require 'core/sanity)
(require 'core/keybindings)

;; Set up the theme.
;; This comes before other stuff to avoid pop-in, though with native-comp
;; it's negligible.
(require 'ui/dashboard)
(require 'ui/fonts)
(require 'ui/misc)
(require 'ui/modeline)
(require 'ui/theme)

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
;; [HACK: Vera; 2023-05-30] 8roken.
;; (require 'tools/org)
(require 'tools/pdf)

;; Set up the programming languages I write.
;; [HACK: Vera; 2023-05-30] 8roken.
;; (require 'lang/agda)
(require 'lang/cooltt)
(require 'lang/coq)
(require 'lang/emacs-lisp)
(require 'lang/haskell)
(require 'lang/nix)
(require 'lang/latex)
(require 'lang/ocaml)
(require 'lang/racket)
(require 'lang/rust)
(require 'lang/sage)

(provide 'init)
;;; init.el ends here
