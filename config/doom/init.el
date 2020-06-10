;;; init.el -*- lexical-binding: t; -*-
;;  ,;' `:.   NOTE: THERE ARE TWO BFGS IN HELL
;; ::  _  ;;
;;  `:_|_;'
;;     !

;; gccemacs
(setq comp-async-env-modifier-form '((setenv "LIBRARY_PATH"
                                             (concat
                                              (shell-command-to-string "nix eval --raw '(let pkgs = import <nixos-unstable> {}; in (pkgs.lib.getLib pkgs.libgccjit + /lib/gcc/x86_64-unknown-linux-gnu/9.3.0:))'")
                                              (shell-command-to-string "nix eval --raw '(let pkgs = import <nixpkgs> {}; in (pkgs.lib.getLib pkgs.stdenv.cc.cc + /lib))'")
                                              (shell-command-to-string "nix eval --raw '(let pkgs = import <nixpkgs> {}; in (pkgs.lib.getLib pkgs.stdenv.glibc + /lib))'")))))

(doom! :completion
       company
       (ivy +icons +prescient)

       :ui
       doom
       doom-dashboard
       doom-quit
       hl-todo
       modeline
       nav-flash
       ophints
       (popup +all +defaults)
       treemacs
       vc-gutter
       vi-tilde-fringe
       window-select
       workspaces
       zen

       :editor
       (evil +everywhere)
       file-templates
       fold
       multiple-cursors
       parinfer
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
       spell     ; ...is this annoying? yes! do i need it for editing latex papers?
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
       common-lisp
       data
       emacs-lisp
       go
       (haskell +dante)
       (java +lsp)
       javascript
       latex
       markdown
       nix
       (org +dragndrop +pandoc +present)
       python
       racket
       (rust +lsp)
       (scala +lsp)
       scheme
       sh

       :email
       (mu4e +gmail)

       :app
       irc
       (rss +org)

       :config
       literate
       (default +bindings +smartparens))
