;;; early-init --- -*- lexical-binding: t; -*-

;;; Commentary:
;; Stuff that runs before everything else.

;;; Code:
;; Defer garbage collection further back in the startup process.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6

      ;; Don't load package.el.
      package-enable-at-startup nil

      ;; Prevent compiling packages at runtime.
      comp-deferred-compilation nil
      native-comp-deferred-compilation nil
      package-native-compile t

      ;; Disable the built-in modeline.
      mode-line-format nil

      ;; Don't yell about cl. I can't control it.
      byte-compile-warnings '(cl-functions)

      ;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
      default-frame-alist
      '((vertical-scroll-bars . nil)
        (menu-bar-lines . 0)
        (tool-bar-lines . 0))
      frame-inhibit-implied-resize t)

;; Ignore .Xresources.
(advice-add #'x-apply-session-resources :override #'ignore)

(provide 'early-init)
;;; early-init.el ends here
