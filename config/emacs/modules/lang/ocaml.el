;;; ocaml.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; It sure is.

;;; Code:
(require 'display-fill-column-indicator)
(require 'use-package)
(require 'core/keybindings)
(require 'editor/checker)

(use-package tuareg
  :straight t
  :init
  :config
  (add-hook 'tuareg-mode-hook (lambda ()
                                (set-fill-column 100)
                                (display-fill-column-indicator-mode)))
  
  (defun tulips/ocaml-switch-interface ()
    "Switch between an interface file, and it's implementation."
    (interactive)
    (let* ((fname (buffer-file-name))
           (ext (pcase (file-name-extension fname)
                  ("ml" "mli")
                  ("mli" "ml")
                  (_ (error "Cannot find interface file for %s" fname)))))
      (find-file (concat (file-name-sans-extension fname) "." ext))))

  (defun tulips/ocaml-menhir-grammar-conflicts ()
    "Open up a the .conflicts file for the current .mly file."
    (interactive)
    (let* ((project-root (projectile-project-root))
           (conflicts-path (f-swap-ext (f-join project-root "_build/default" (f-relative (buffer-file-name) project-root)) "conflicts")))
      (find-file conflicts-path)))

  (defun tulips/ocaml-open-docs-sentinel (process _)
    "Process sentinel for `ocaml-open-docs'."
    (unless (process-live-p process)
      ;; NOTE: We mask of any errors here to avoid getting in a weird state.
      (ignore-errors
        (with-current-buffer (process-buffer process)
          (goto-char (point-max))
          (let ((packages nil))
            (while (re-search-backward "^\\([^[:space:]]*\\) .*$" nil t)
              (push (match-string 1) packages))
            (make-process :name "odig doc"
                          :noquery t
                          :command (list "odig" "doc" "--background" (completing-read "Package: " packages)))))
        (kill-buffer (process-buffer process)))))

  (defun tulips/ocaml-open-docs ()
    "Open the document page for a package."
    (interactive)
    (make-process :name "odig"
                  :buffer (generate-new-buffer "*odig*")
                  :noquery t
                  :command '("odig" "pkg")
                  :sentinel 'ocaml-open-docs-sentinel))

  :general
  (mode-leader-definer
    :keymaps 'tuareg-mode-map
    "s" '(tulips/ocaml-switch-interface :wk "switch interface"))

  (mode-leader-definer
    :keymaps 'tuareg-menhir-mode-map
    "s" '(tulips/ocaml-menhir-grammar-conflicts :wk "conflicts"))

  (global-motion-definer
    :keymaps 'utop-mode-map
    "k" '(utop-history-goto-prev :wk "previous")
    "j" '(utop-history-goto-next :wk "next")))

(use-package merlin
  :straight t
  :ensure t
  :after tuareg
  :config
  (add-hook 'tuareg-mode-hook 'merlin-mode)
  (add-hook 'merlin-mode-hook (lambda () (setq-local evil-lookup-func 'merlin-document)))
  :general
  (mode-leader-definer
    :keymaps 'merlin-mode-map
    "c" '(merlin-destruct :wk "case split")
    "t" '(merlin-type-enclosing :wk "type")
    "r" '(merlin-error-check :wk "type")
    "o" '(merlin-occurrences :wk "occurances"))

  (error-menu-definer
    :keymaps 'merlin-mode-map
    "j" '(merlin-error-next :wk "next error")
    "k" '(merlin-error-prev :wk "prev error")))

(use-package merlin-company
  :straight t
  :ensure t
  :init
  (add-hook 'merlin-mode-hook 'company-mode))

(use-package merlin-eldoc
  :straight t
  :ensure t
  :init
  (add-hook 'merlin-mode-hook 'merlin-eldoc-setup))

(use-package ocp-indent
  :straight t
  :after tuareg
  :config
  ;; ocp-indent-buffer is broken...
  (advice-add 'ocp-indent-buffer
              :override (lambda (&rest r)
                          (ocp-indent-region (point-min) (point-max))))

  (add-hook 'tuareg-mode-hook
            (lambda () (add-hook 'before-save-hook 'ocp-indent-buffer nil 'local))))

(use-package utop
  :straight t
  :after tuareg
  :config
  (setq utop-command "opam config exec -- dune utop . -- -emacs")

  :general
  (mode-leader-definer
    :keymaps 'tuareg-mode-map
    "l" '(utop-eval-buffer :wk "load"))
  (global-motion-definer
    :keymaps 'utop-mode-map
    "k" '(utop-history-goto-prev :wk "previous")
    "j" '(utop-history-goto-next :wk "next")))

(defun tulips/dune-watch-filter (proc string)
  "Process filter for `dune-watch'"
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t)
            (moving (= (point) (process-mark proc))))
        (if (s-matches? "NEW BUILD" string)
            (erase-buffer)
          (save-excursion
            (goto-char (process-mark proc))
            (insert string)
            (set-marker (process-mark proc) (point)))
          (if moving (goto-char (process-mark proc))))))))

(defun tulips/dune-watch ()
  "Run `dune build --watch' for the current project."
  (interactive)
  (let ((default-directory (projectile-project-root))
        (proc-buffer (generate-new-buffer "*dune-watch*")))
    (with-current-buffer proc-buffer
      (compilation-mode 1))
    (display-buffer proc-buffer)
    (make-process
     :name "dune-watch"
     :buffer proc-buffer
     :command '("dune" "build" "@install" "--watch")
     :filter #'dune-watch-filter)))


(provide 'lang/ocaml)
;;; ocaml.el ends here
