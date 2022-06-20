;;; lang/latex ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; Overfull \hbox (badness 10000)

;;; Code:
(require 'use-package)
(require 'core/keybindings)

(use-package auctex
  :straight t
  :config
  (add-hook 'LaTeX-mode-hook #'flyspell-mode)
  (add-hook 'LaTeX-mode-hook
	    (lambda ()
	      (setq TeX-command-extra-options "-shell-escape")))

  :config 
  (defun tulips/texcount ()
    "Counts words in a TeX file."
    (interactive)
    (let*
	((this-file (buffer-file-name))
	 (enc-str (symbol-name buffer-file-coding-system))
	 (enc-opt
          (cond
           ((string-match "utf-8" enc-str) "-utf8")
           ((string-match "latin" enc-str) "-latin1")
           ("-encoding=guess")))
	 (word-count
          (with-output-to-string
            (with-current-buffer standard-output
              (call-process "texcount" nil t nil "-0" enc-opt this-file)))))
      (message word-count))) 

  :general
  (mode-leader-definer
    :keymaps 'LaTeX-mode-map
    "c" '(TeX-command-master :wk "compile")
    "e" '(LaTeX-environment :wk "environment")
    "i" '(LaTeX-insert-item :wk "item")
    "w" '(tulips/texcount :wk "word count"))

  (error-menu-definer
    :keymaps 'LaTeX-mode-map
    "j" '(TeX-next-error :wk "next error")
    "k" '(TeX-previous-error :wk "previous error"))

  (mode-leader-definer
    :keymaps 'flyspell-mode-map
    "s" '(flyspell-correct-word-before-point :wk "spell check")))

(use-package evil-tex
  :straight t
  :after auctex)

(use-package auctex-latexmk
  :straight t
  :after auctex
  :config
  (auctex-latexmk-setup)
  (setq auctex-latexmk-inherit-TeX-pdf-mode t))

(provide 'lang/latex)
;;; latex.el ends here
