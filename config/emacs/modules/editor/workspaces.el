;;; workspaces.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; Workspaces a la DOOM, with magic tab bar hacks.
;; 
;; The only caveat to this approach is that buffers from one workspace
;; are visible in another, which is a blessing and a curse. This isn't
;; the case in DOOM, because persp workspaces are separate entities.

;;; Code:
(require 'core/keybindings)

;; Don't show the tab bar, but do the things the tab bar does.
;; Everything's binded, and the current tab shows in the modeline.
(setq tab-bar-show nil)

;; Set the first tab name.
(tab-bar-rename-tab "main")

;; Open the dashboard upon loading a new tab.
(setq tab-bar-new-tab-choice
      (lambda ()
        (get-buffer-create "*dashboard*")))

;; This generates the new workspace name from its index, because
;; otherwise it's named to the name of the focused buffer, which gets
;; confusing real fast.
(defun tulips/new-workspace ()
  "Makes a new workspace, with a numerical name."
  (interactive)
  (let ((ntabs (length (tab-bar-tabs))))
    (tab-bar-new-tab)
    (tab-bar-rename-tab (format "%d" (1+ ntabs)))))

;; Make workspace switcher functions.
(dotimes (i 9)
  (let ((i (1+ i)))
    (defalias (intern (format "tulips/switch-to-workspace-%d" i))
      (lambda ()
        (interactive)
        (let ((ntabs (length (tab-bar-tabs))))
          (if (<= i ntabs)
              (tab-select i)
            (error "Workspace %d does not exist" i))))
      (format "Switch to workspace %d" i))))

(general-create-definer workspace-leader-definer
  :keymaps 'override
  :states '(normal motion)
  :wrapping global-definer
  :prefix "SPC TAB"
  "" '(:ignore t :wk "workspaces"))

(workspace-leader-definer
  "1" '(tulips/switch-to-workspace-1 :wk "to 1")
  "2" '(tulips/switch-to-workspace-2 :wk "to 2")
  "3" '(tulips/switch-to-workspace-3 :wk "to 3")
  "4" '(tulips/switch-to-workspace-4 :wk "to 4")
  "5" '(tulips/switch-to-workspace-5 :wk "to 5")
  "6" '(tulips/switch-to-workspace-6 :wk "to 6")
  "7" '(tulips/switch-to-workspace-7 :wk "to 7")
  "8" '(tulips/switch-to-workspace-8 :wk "to 8")
  "9" '(tulips/switch-to-workspace-9 :wk "to 9")
  "d" '(tab-bar-close-tab :wk "delete")
  "n" '(tulips/new-workspace :wk "new")
  "r" '(tab-bar-rename-tab :wk "rename")
  "." '(tab-bar-switch-to-tab :wk "switch to"))

(provide 'editor/workspaces)
;;; workspaces.el ends here
