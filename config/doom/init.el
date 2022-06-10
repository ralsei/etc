;; init.el -*- lexical-binding: t; -*-
;;  ,;' `:.   NOTE: THERE ARE TWO BFGS IN HELL
;; ::  _  ;;
;;  `:_|_;'
;;     !

(doom! :completion
       company
       (vertico +icons)

       :ui
       doom
       doom-dashboard
       doom-quit
       hl-todo
       ligatures
       modeline
       nav-flash
       ophints
       (popup +all +defaults)
       treemacs
       tree-sitter
       vc-gutter
       vi-tilde-fringe
       window-select
       workspaces
       zen

       :editor
       (evil +everywhere)
       file-templates
       ;; lispy
       rotate-text
       snippets

       :emacs
       (dired +icons +ranger)
       electric
       ibuffer
       vc

       :term
       eshell
       vterm

       :checkers
       syntax
       (spell +flyspell)  ; ...is this annoying? yes! do i need it for editing latex papers?
                                        ; also yes!

       :tools
       debugger
       direnv
       (eval +overlay)
       gist
       (lookup +docsets)
       lsp
       magit
       make
       pdf
       rgb

       :lang
       cc
       data
       emacs-lisp
       (haskell +lsp)
       (java +lsp)
       javascript
       julia
       latex
       markdown
       nix
       (org +dragndrop
            +hugo
            +pandoc
            +present
            +pretty)
       python
       (racket +xp)
       (rust +lsp)
       (scala +lsp)

       :app
       irc
       (rss +org)

       :config
       literate
       (default +bindings +smartparens))
