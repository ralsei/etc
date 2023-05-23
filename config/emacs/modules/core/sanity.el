;;; sanity.el --- -*- lexical-binding: t; -*-

;;; Commentary:
;; Make stuff not misbehave outright.

;;; Code:
(require 'use-package)

;; Don't generate the godawful autosave files.
(setq auto-save-default nil)
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))
(setq create-lockfiles nil)

;; Scroll line-by-line.
(setq scroll-conservatively 101)
(setq mouse-wheel-scroll-amount '(1))
(setq mouse-wheel-progressive-speed nil)
(setq redisplay-skip-fontification-on-input t)

;; Make yes/no prompts use y/n instead.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Go fast.
(setq native-comp-speed 2)

;; Stop yelling about warnings.
(setq warning-minimum-level :error)

;; Set up $PATH.
(use-package exec-path-from-shell
  :straight t
  :config
  (exec-path-from-shell-initialize))

;; Use spaces, not tabs.
(defun tulips/generate-tab-stops (&optional max)
  "Return a sequence suitable for `tab-stop-list'."
  (let* ((max-column (or max 200))
         (count (/ max-column tab-width)))
    (number-sequence tab-width (* tab-width count) tab-width)))

(setq tab-width 4)
(setq tab-stop-list (tulips/generate-tab-stops 4))
(setq-default indent-tabs-mode nil)

;; Important.
(defun tulips/insert-shruggie ()
  "Insert an ASCII shrug."
  (interactive)
  (insert "¯\\_(ツ)_/¯"))

(defun tulips/insert-table-flip ()
  "Insert an ASCII table flip."
  (interactive)
  (insert "(╯°□°）╯︵ ┻━┻)"))

;; Print a buffer to PDF.
(require 'ps-print)
(defun tulips/pdf-print-buffer-with-faces (&optional filename)
  "Print file in the current buffer as pdf, including font, color, and
underline information.  This command works only if you are using a window system,
so it has a way to determine color values.

C-u COMMAND prompts user where to save the Postscript file (which is then
converted to PDF at the same location."
  (interactive (list (if current-prefix-arg
                         (ps-print-preprint 4)
                       (concat (file-name-sans-extension (buffer-file-name))
                               ".ps"))))
  (ps-print-with-faces (point-min) (point-max) filename)
  (shell-command (concat "ps2pdf " filename))
  (delete-file filename)
  (message "Deleted %s" filename)
  (message "Wrote %s" (concat (file-name-sans-extension filename) ".pdf")))

(provide 'core/sanity)
;;; sanity.el ends here
