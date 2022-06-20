;;; tools/org --- -*- lexical-binding: t; -*-

;;; Commentary:
;; I haven't used Org much in the past.  Hopefully this deals with
;; my rampant memory issues.
;;
;; I'm trying explicitly not to bikeshed too much here.
;; I don't use Org for LaTeX.  Yet.

;;; Code:
(require 'use-package)
(require 'core/keybindings)

(general-create-definer notes-menu-definer
  :states '(normal motion)
  :wrapping global-definer
  :prefix "SPC n"
  "" '(:ignore t :wk "notes"))

(use-package org
  :straight t
  :init
  ;; Sync to the cloud for Orglzy.
  (setq org-directory "~/usr/cloud/org")

  (add-hook 'org-mode-hook 'auto-fill-mode)

  (setq org-todo-keywords '((sequence
                             "TODO(t)"
                             "STRT(s)"
                             "WAIT(w)"
                             "BLCK(b)"
                             "|"
                             "DONE(d)"
                             "KILL(k)")))

  (custom-declare-face 'org-todo-active '((t (:inherit (bold font-lock-string-face org-todo)))) "")
  (custom-declare-face 'org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
  (custom-declare-face 'org-todo-onhold '((t (:inherit (bold warning org-todo)))) "")
  (custom-declare-face 'org-todo-blocked '((t (:inherit (bold error org-todo)))) "")
  (custom-declare-face 'org-todo-idea '((t (:inherit (bold font-lock-builtin-face org-todo)))) "")

  (setq org-startup-folded 'content)
  (setq org-startup-indented t)

  ;; Pretty.
  (setq org-bullets-bullet-list '("☯" "☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷"))
  (setq org-ellipsis "↝")

  :config
  (evil-collection-org-setup)

  (defun tulips/find-task-file ()
    "Open an Org task file."
    (interactive)
    (let ((default-directory org-directory))
      (call-interactively 'find-file)))

  :general
  (mode-leader-definer
    :keymaps 'org-mode-map
    "l" '(org-insert-link :wk "insert link")
    "e" '(org-export-dispatch :wk "export")
    "o" '(org-open-at-point :wk "open at point")
    "t" '(org-todo :wk "change todo state")
    "w" '(org-refile :wk "refile")
    "'" '(org-edit-special :wk "edit block"))

  (mode-leader-definer
    :keymaps 'org-capture-mode-map
    "c" '(org-capture-finalize :wk "capture")
    "k" '(org-capture-kill :wk "cancel"))

  (general-create-definer org-set-definer
    :wrapping mode-leader-definer
    :keymaps 'org-mode-map
    :prefix "SPC m x"
    "" '(:ignore t :wk "set")
    "p" '(org-set-property :wk "set property"))

  (general-create-definer org-clock-definer
    :wrapping mode-leader-definer
    :keymaps 'org-mode-map
    :prefix "SPC m c"
    "" '(:ignore t :wk "clock")
    "i" '(org-clock-in :wk "clock in")
    "o" '(org-clock-out :wk "clock out")
    "e" '(org-set-effort :wk "effort"))

  (notes-menu-definer
    "l" '(org-store-link :wk "store link")
    "o" '(org-clock-goto :wk "open current task")
    "s" '(org-clock-in :wk "clock in")
    "S" '(org-clock-out :wk "clock out")))

(use-package org-roam
  :straight t
  :init
  (setq org-roam-directory "~/usr/cloud/org-roam")
  (setq org-roam-dailies-directory "daily/")

  (setq org-roam-dailies-capture-templates
	'(("d" "default" entry
	   "* %?"
	   :target (file+head "%<%Y-%m-%d>.org"
			      "#+title: %<%Y-%m-%d>\n"))))

  (setq org-roam-v2-ack t)
  (org-roam-db-autosync-mode)

  :config
  (defun tulips/org-roam-open-todo ()
    "Open the org roam agenda file."
    (interactive)
    (org-roam-node-visit (org-roam-node-from-title-or-alias "Agenda")))

  :general
  (mode-leader-definer
    :keymaps 'org-mode-map
    "n" '(org-roam-node-insert :wk "roam insert"))

  (notes-menu-definer
    "n" '(org-roam-node-find :wk "open note")
    "d" '(org-roam-dailies-goto-today :wk "open daily")
    "t" '(tulips/org-roam-open-todo :wk "open todos")
    "c" '(org-roam-capture :wk "capture")))

(provide 'tools/org)
;;; org.el ends here
