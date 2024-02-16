;;; coq.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; AUGH. 8LAUGH.

;;; Code:
(require 'core/keybindings)

(use-package proof-general
  :straight t
  :config
  ; [NOTE: Vera; 2023-05-29] No anime.
  (setq proof-splash-enable nil)
  :general
  (mode-leader-definer 'coq-mode-map
    "l" '(proof-process-buffer :wk "load")
    "r" '(proof-retract-buffer :wk "retract")
    "j" '(proof-assert-next-command-interactive :wk "assert next")
    "k" '(proof-undo-last-successful-command :wk "undo command")
    "." '(proof-assert-until-point-interactive :wk "assert to point")
    "," '(proof-retract-until-point-interactive :wk "retract to point"))
  (global-motion-definer
    :keymaps 'coq-mode-map
    "j" '(proof-assert-next-command-interactive :wk "assert next")
    "k" '(proof-undo-last-successful-command :wk "undo command")))

(use-package company-coq
  :straight t
  :config
  (add-hook 'coq-mode-hook #'company-coq-mode))

(provide 'lang/coq)
;;; coq.el ends here
