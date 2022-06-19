;;; agda.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; Agony.  Pain, even.

;;; Code:
(require 'core/keybindings)
(require 'editor/projectile)
(require 'editor/snippets)

;; It's recommended to load agda-mode from the disk, rather than loading
;; from straight.el, because that way it stays in sync with your local version.
(defun tulips/find-agda-mode ()
  "Determine the location of the `agda2-mode' elisp files on your system."
  (condition-case _ (with-temp-buffer (call-process "agda-mode" nil t nil "locate")
                                      (buffer-string))
      (error (error "Could not find the `agda-mode' binary in your path.  Do you have agda installed?"))))

(load (tulips/find-agda-mode))

(add-to-list 'auto-mode-alist '("\\.lagda.md\\'" . agda2-mode))
(create-file-template ".*.lagda.md$" "lagda-template" 'agda2-mode)
(add-to-list 'projectile-globally-ignored-file-suffixes ".agdai")

(with-eval-after-load "agda2-mode"
  (require 'agda2-mode)

  (defun tulips/agda2-goal-and-context-normalized ()
    (interactive)
    (agda2-goal-and-context 4))

  (defun tulips/agda2-goal-and-context-and-inferred-normalized ()
    (interactive)
    (agda2-goal-and-context-and-inferred 4))

  (mode-leader-definer
    :keymaps 'agda2-mode-map
    "a" '(agda2-auto-maybe-all :wk "auto")
    "c" '(agda2-make-case :wk "case split")
    "l" '(agda2-load :wk "load")
    "n" '(agda2-compute-normalised-maybe-toplevel :wk "normalize")
    "e" '(agda2-elaborate-give :wk "elaborate")
    "i" '(agda2-search-about-toplevel :wk "info")
    "r" '(agda2-refine :wk "refine")
    "s" '(agda2-solve-maybe-all :wk "solve")
    "w" '(agda2-why-in-scope-maybe-toplevel :wk "describe scope")
    "o" '(agda2-module-contents-maybe-toplevel :wk "module contents")
    "," '(agda2-goal-and-context :wk "display goal")
    "<" '(tulips/agda2-goal-and-context-normalized :wk "display goal (normalized)")
    "." '(agda2-goal-and-context-and-inferred :wk "display type")
    ">" '(tulips/agda2-goal-and-context-and-inferred-normalized :wk "display type (normalized)"))

  (global-motion-definer
    :keymaps 'agda2-mode-map
    "d" '(agda2-goto-definition-keyboard :wk "goto definition")
    "j" '(agda2-next-goal :wk "next goal")
    "k" '(agda2-previous-goal :wk "previous goal")))

(with-eval-after-load 'agda-input
  ;; Needed to silence the byte compiler.
  (declare-function agda-input-add-translations "agda-input")
  (agda-input-add-translations '(("hom" . "⇒")
				 ("lam" . "λ")
				 ("lam-" . "ƛ")
				 ("iso" . "≅")
				 ("embed" . "↪")
				 ("mono" . "↣")
				 ("epi" . "↠")
				 ("nat" . "ℕ")
				 ("int" . "ℤ")
				 ("alpha" . "α")
				 ("beta" . "β")
				 ("gamma" . "γ")
				 ("yo" . "よ")
				 ("inv" . "⁻¹")
				 ("monus" . "∸")
				 ("uu" . "⇑"))))

(require 'agda-input)

(provide 'lang/agda)
;;; agda.el ends here
