;;; qi-mode-el -- Major mode for editing Qi files
;;; qi-inferior-mode provided below

;; Author: Michael Ilseman
;; Created: 12 May 2007
;; Keywords: Qi major-mode

;; Copyright (C) 2007 Michael Ilseman

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:
;; based on the information from the tutorial below
;; and the scheme major mode
;;
;; http://two-wugs.net/emacs/mode-tutorial.html
;; 
;; Contributers: Jt Gleason <jt@entropyfails.com>

;;; Code:
(defvar qi-mode-hook nil)
(defvar qi-mode-map nil)

(unless qi-mode-map
  (let ((map (make-sparse-keymap "Qi")))
    (setq qi-mode-map (make-sparse-keymap))
    (define-key qi-mode-map "\C-c\C-c" 'run-qi)
    (set-keymap-parent qi-mode-map lisp-mode-shared-map)
    (define-key qi-mode-map [menu-bar] (make-sparse-keymap))
    (define-key qi-mode-map [menu-bar qi]
      (cons "Qi" map))
    (define-key map [run-qi] '("Run Inferior Qi" . run-qi))
    (define-key map [indent-region] '("Indent Region" . indent-region))
    (define-key map [indent-line] '("Indent Line" . lisp-indent-line))
    (put 'comment-region 'menu-enable 'mark-active)
    (put 'uncomment-region 'menu-enable 'mark-active)
    (put 'indent-region 'menu-enable 'mark-active)))

(add-to-list 'auto-mode-alist '("\\.qi\\'" . qi-mode))

(defconst qi-font-lock-keywords-1
  (list
   '("\\<\\(define\\|datatype\\)\\>" . font-lock-keyword-face)
   '("\\('\\w*'\\)" . font-lock-variable-name-face))
  "Minimal highlighting expressions for Qi mode.")

(defconst qi-font-lock-keywords-2
  (append qi-font-lock-keywords-1
          (list
           '("^error: .*$" . font-lock-warning-face)
           '("\\<\\(if\\|tc\\)\\>" . font-lock-function-name-face)
           '("\\W/\\.\\W" . font-lock-function-name-face)
           '("\\<\\([A-Z]\\w*\\)\\>" . font-lock-variable-name-face)
           '("\\<where\\>" . font-lock-type-face)
           '("\\W@p\\W" . font-lock-type-face)
           '("\\<\\(boolean[?]\\|character[?]\\|complex[?]\\|congruent[?]\\|cons[?]\\|element[?]\\|empty[?]\\|float[?]\\|integer[?]\\|number[?]\\|provable[?]\\|rational[?]\\|solved[?]\\|string[?]\\|symbol[?]\\|tuple[?]\\|variable[?]\\)\\W" . font-lock-builtin-face)
           '("\\<\\(and\\|append\\|apply\\|atp-credits\\|atp-prompt\\|cd\\|collect\\|concat\\|cons\\|delete-file\\|destroy\\|debug\\|difference\\|display-mode\\|do\\|dump\\|dump-proof\\|eval\\|explode\\|error\\|fix\\|from-goals\\|fst\\|fst-ass\\|fst-conc\\|fst-goal\\|gensym\\|get-array\\|get-prop\\|get-rule\\|head\\|if-with-checking\\|if-without-checking\\|include\\|include-all-but\\|inferences\\|input\\|length\\|lineread\\|map\\|macroexpand\\|make-string\\|maxinferences\\|newfuntype\\|notes-in\\|nth\\|occurrences\\|output\\|preclude\\|preclude-all-but\\|prf\\|profile\\|profile-results\\|prooftool\\|put-array\\|put-prop\\|quit\\|random\\|read-char\\|read-file\\|read-file-as-charlist\\|read-chars-as-stringlist\\|refine\\|reserve\\|reverse\\|round\\|save\\|snd\\|spy\\|sqrt\\|step\\|strong-warning\\|tail\\|theory-size\\|thm-intro\\|to-goals\\|time\\|time-proof\\|track\\|undebug\\|union\\|unprf\\|unprofile\\|unreserve\\|unspecialise\\|untrack\\|value\\|version\\|warn\\|write-to-file\\)\\>" . font-lock-builtin-face)
;           '("\\<test\\>"  . font-lock-comment-face)
;           '("\\<test1\\>" . font-lock-keyword-face)
;           '("\\<test3\\>" . font-lock-variable-name-face)
;           '("\\<test4\\>" . font-lock-type-face)
;           '("\\<test5\\>" . font-lock-constant-face)
;           '("\\<test6\\>" . font-lock-warning-face)
           ))
  "more highlighting in Qi mode.")

(defconst qi-font-lock-keywords-3
  (append qi-font-lock-keywords-2
          (list
           '("\\<\\(true\\|false\\|character\\|string\\|symbol\\|number\\|list\\|boolean\\)\\>" . font-lock-constant-face)))
  "Additional Keywords to highlight in Qi mode.")
(defvar qi-font-lock-keywords qi-font-lock-keywords-3
  "Default highlighting expressions for Qi mode.")

;; Copied from scheme-mode, which in turn is from lisp-mode
(defun qi-indent-function (indent-point state)
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
					 calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let ((function (buffer-substring (point)
					(progn (forward-sexp 1) (point))))
	    method)
	(setq method (or (get (intern-soft function) 'qi-indent-function)
			 (get (intern-soft function) 'qi-indent-hook)))
	(cond ((or (eq method 'defun)
		   (and (null method)
			(> (length function) 3)
			(string-match "\\`def" function)))
	       (lisp-indent-defform state indent-point))
	      ((integerp method)
	       (lisp-indent-specform method state
				     indent-point normal-indent))
	      (method
		(funcall method state indent-point normal-indent)))))))


(defun qi-let-indent (state indent-point normal-indent)
  (skip-chars-forward " \t")
  (if (looking-at "[-a-zA-Z0-9+*/?!@$%^&_:~]")
      (lisp-indent-specform 2 state indent-point normal-indent)
    (lisp-indent-specform 1 state indent-point normal-indent)))

;; (put 'begin 'qi-indent-function 0), say, causes begin to be indented
;; like defun if the first form is placed on the next line, otherwise
;; it is indented like any other form (i.e. forms line up under first).

;(put (make-symbol "/.") 'qi-indent-function 1)
(put 'datatype 'qi-indent-function 1)
(put 'let 'qi-indent-function 'qi-let-indent)
;(put (make-symbol "->") 'qi-indent-function 'qi-let-indent)


;; Qi uses the \ character as the comment delimiter
;; Qi follows lisp in using # as the character escape, this overrides the comment type for #\Space sequences
(defvar qi-mode-syntax-table
  (let ((qi-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?\\ "!" qi-mode-syntax-table)
    (modify-syntax-entry ?# "\\" qi-mode-syntax-table)
    (modify-syntax-entry ?- "w" qi-mode-syntax-table)
    qi-mode-syntax-table)
  "Syntax table for qi-mode")
  
(defun qi-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map qi-mode-map)
  (set-syntax-table qi-mode-syntax-table)
  ;; Set up font-lock
  (set (make-local-variable 'font-lock-defaults) '(qi-font-lock-keywords))
  ;; Register our indentation function
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'lisp-indent-line)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'lisp-comment-indent)
  (make-local-variable 'lisp-indent-function)
  (set lisp-indent-function 'qi-indent-function)
  (setq major-mode 'qi-mode)
  (setq mode-name "Qi")
  (run-hooks 'qi-mode-hook))

(provide 'qi-mode)

;;; inferior-qi-mode --- an inferior-qi mode

;; Copyright (C) 2007

;; Author: Michael Ilseman 
;; Keywords: processes, qi

;;; Commentary:

;; Mostly taken from inf-lisp.el. Pretty much a copy/paste,
;; search/replace with syntax highlighting added.

;; This file defines a qi-in-a-buffer package (inferior-qi mode)
;; built on top of comint mode.  

;; Since this mode is built on top of the general command-interpreter-in-
;; a-buffer mode (comint mode), it shares a common base functionality,
;; and a common set of bindings, with all modes derived from comint mode.
;; This makes these modes easier to use.

;; For documentation on the functionality provided by comint mode, and
;; the hooks available for customising it, see the file comint.el.
;; For further information on inferior-qi mode, see the comments below.

;; Needs fixin:

;;; Code:

(require 'comint)
(require 'qi-mode)


;;;###autoload
(defvar inferior-qi-filter-regexp "\\`\\s *\\(:\\(\\w\\|\\s_\\)\\)?\\s *\\'"
  "*What not to save on inferior Qi's input history.
Input matching this regexp is not saved on the input history in Inferior Qi
mode.  Default is whitespace followed by 0 or 1 single-letter colon-keyword
\(as in :a, :c, etc.)")

(defvar inferior-qi-mode-map nil)
(unless inferior-qi-mode-map
  (setq inferior-qi-mode-map (copy-keymap comint-mode-map))
;  (set-keymap-parent inferior-qi-mode-map qi-mode-shared-map)
  (define-key inferior-qi-mode-map "\C-x\C-e" 'qi-eval-last-sexp)
  (define-key inferior-qi-mode-map "\C-c\C-l" 'qi-load-file)
  (define-key inferior-qi-mode-map "\C-c\C-k" 'qi-compile-file)
  (define-key inferior-qi-mode-map "\C-c\C-a" 'qi-show-arglist)
  (define-key inferior-qi-mode-map "\C-c\C-d" 'qi-describe-sym)
  (define-key inferior-qi-mode-map "\C-c\C-f"
    'qi-show-function-documentation)
  (define-key inferior-qi-mode-map "\C-c\C-v"
    'qi-show-variable-documentation))

;;; These commands augment Qi mode, so you can process Qi code in
;;; the source files.
(define-key qi-mode-map "\M-\C-x"  'qi-eval-defun)     ; Gnu convention
(define-key qi-mode-map "\C-x\C-e" 'qi-eval-last-sexp) ; Gnu convention
(define-key qi-mode-map "\C-c\C-e" 'qi-eval-defun)
(define-key qi-mode-map "\C-c\C-r" 'qi-eval-region)
(define-key qi-mode-map "\C-c\C-c" 'qi-compile-defun)
(define-key qi-mode-map "\C-c\C-z" 'switch-to-qi)
(define-key qi-mode-map "\C-c\C-l" 'qi-load-file)
(define-key qi-mode-map "\C-c\C-k" 'qi-compile-file)  ; "kompile" file
(define-key qi-mode-map "\C-c\C-a" 'qi-show-arglist)
(define-key qi-mode-map "\C-c\C-d" 'qi-describe-sym)
(define-key qi-mode-map "\C-c\C-f" 'qi-show-function-documentation)
(define-key qi-mode-map "\C-c\C-v" 'qi-show-variable-documentation)


;;; This function exists for backwards compatibility.
;;; Previous versions of this package bound commands to C-c <letter>
;;; bindings, which is not allowed by the gnumacs standard.

;;;  "This function binds many inferior-qi commands to C-c <letter> bindings,
;;;where they are more accessible. C-c <letter> bindings are reserved for the
;;;user, so these bindings are non-standard. If you want them, you should
;;;have this function called by the inferior-qi-load-hook:
;;;    (setq inferior-qi-load-hook '(inferior-qi-install-letter-bindings))
;;;You can modify this function to install just the bindings you want."
(defun inferior-qi-install-letter-bindings ()
  (define-key qi-mode-map "\C-ce" 'qi-eval-defun-and-go)
  (define-key qi-mode-map "\C-cr" 'qi-eval-region-and-go)
  (define-key qi-mode-map "\C-cc" 'qi-compile-defun-and-go)
  (define-key qi-mode-map "\C-cz" 'switch-to-qi)
  (define-key qi-mode-map "\C-cl" 'qi-load-file)
  (define-key qi-mode-map "\C-ck" 'qi-compile-file)
  (define-key qi-mode-map "\C-ca" 'qi-show-arglist)
  (define-key qi-mode-map "\C-cd" 'qi-describe-sym)
  (define-key qi-mode-map "\C-cf" 'qi-show-function-documentation)
  (define-key qi-mode-map "\C-cv" 'qi-show-variable-documentation)

  (define-key inferior-qi-mode-map "\C-cl" 'qi-load-file)
  (define-key inferior-qi-mode-map "\C-ck" 'qi-compile-file)
  (define-key inferior-qi-mode-map "\C-ca" 'qi-show-arglist)
  (define-key inferior-qi-mode-map "\C-cd" 'qi-describe-sym)
  (define-key inferior-qi-mode-map "\C-cf" 'qi-show-function-documentation)
  (define-key inferior-qi-mode-map "\C-cv"
    'qi-show-variable-documentation))


;;;###autoload
(defvar inferior-qi-program "Qi"
  "*Program name for invoking an inferior Qi with for Inferior Qi mode.")

;;;###autoload
(defvar inferior-qi-load-command "(load \"%s\")\n"
  "*Format-string for building a Qi expression to load a file.
This format string should use `%s' to substitute a file name
and should result in a Qi expression that will command the inferior Qi
to load that file.  The default works acceptably on most Qis.
The string \"(progn (load \\\"%s\\\" :verbose nil :print t) (values))\\n\"
produces cosmetically superior output for this application,
but it works only in Common Qi.")

;;;###autoload
(defvar inferior-qi-prompt "^[^> \n]*>+:? *"
  "Regexp to recognise prompts in the Inferior Qi mode.
Defaults to \"^[^> \\n]*>+:? *\", which works pretty good for Lucid, kcl,
and franz.  This variable is used to initialize `comint-prompt-regexp' in the
Inferior Qi buffer.

This variable is only used if the variable
`comint-use-prompt-regexp-instead-of-fields' is non-nil.

More precise choices:
Lucid Common Qi: \"^\\\\(>\\\\|\\\\(->\\\\)+\\\\) *\"
franz: \"^\\\\(->\\\\|<[0-9]*>:\\\\) *\"
kcl: \"^>+ *\"

This is a fine thing to set in your .emacs file.")

(defvar inferior-qi-buffer nil "*The current inferior-qi process buffer.

MULTIPLE PROCESS SUPPORT
===========================================================================
To run multiple Qi processes, you start the first up
with \\[inferior-qi].  It will be in a buffer named `*inferior-qi*'.
Rename this buffer with \\[rename-buffer].  You may now start up a new
process with another \\[inferior-qi].  It will be in a new buffer,
named `*inferior-qi*'.  You can switch between the different process
buffers with \\[switch-to-buffer].

Commands that send text from source buffers to Qi processes --
like `qi-eval-defun' or `qi-show-arglist' -- have to choose a process
to send to, when you have more than one Qi process around.  This
is determined by the global variable `inferior-qi-buffer'.  Suppose you
have three inferior Qis running:
    Buffer              Process
    foo                 inferior-qi
    bar                 inferior-qi<2>
    *inferior-qi*     inferior-qi<3>
If you do a \\[qi-eval-defun] command on some Qi source code,
what process do you send it to?

- If you're in a process buffer (foo, bar, or *inferior-qi*),
  you send it to that process.
- If you're in some other buffer (e.g., a source file), you
  send it to the process attached to buffer `inferior-qi-buffer'.
This process selection is performed by function `inferior-qi-proc'.

Whenever \\[inferior-qi] fires up a new process, it resets
`inferior-qi-buffer' to be the new process's buffer.  If you only run
one process, this does the right thing.  If you run multiple
processes, you can change `inferior-qi-buffer' to another process
buffer with \\[set-variable].")

;;;###autoload
(defvar inferior-qi-mode-hook '()
  "*Hook for customising Inferior Qi mode.")

(put 'inferior-qi-mode 'mode-class 'special)

(defun inferior-qi-mode ()
  "Major mode for interacting with an inferior Qi process.
Runs a Qi interpreter as a subprocess of Emacs, with Qi I/O through an
Emacs buffer.  Variable `inferior-qi-program' controls which Qi interpreter
is run.  Variables `inferior-qi-prompt', `inferior-qi-filter-regexp' and
`inferior-qi-load-command' can customize this mode for different Qi
interpreters.

For information on running multiple processes in multiple buffers, see
documentation for variable `inferior-qi-buffer'.

\\{inferior-qi-mode-map}

Customisation: Entry to this mode runs the hooks on `comint-mode-hook' and
`inferior-qi-mode-hook' (in that order).

You can send text to the inferior Qi process from other buffers containing
Qi source.
    switch-to-qi switches the current buffer to the Qi process buffer.
    qi-eval-defun sends the current defun to the Qi process.
    qi-compile-defun compiles the current defun.
    qi-eval-region sends the current region to the Qi process.
    qi-compile-region compiles the current region.

    Prefixing the qi-eval/compile-defun/region commands with
    a \\[universal-argument] causes a switch to the Qi process buffer after sending
    the text.

Commands:
Return after the end of the process' output sends the text from the
    end of process to point.
Return before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
Delete converts tabs to spaces as it moves back.
Tab indents for Qi; with argument, shifts rest
    of expression rigidly with the current line.
C-M-q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.  Semicolons start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it."
  (interactive)
  (comint-mode)
  (set (make-local-variable 'font-lock-defaults) '(qi-font-lock-keywords))
  (setq comint-prompt-regexp inferior-qi-prompt)
  (setq major-mode 'inferior-qi-mode)
  (setq mode-name "Inferior Qi")
  (setq mode-line-process '(":%s"))

  (use-local-map inferior-qi-mode-map)    ;c-c c-k for "kompile" file
  (setq comint-get-old-input (function qi-get-old-input))
  (setq comint-input-filter (function qi-input-filter))
  (run-hooks 'inferior-qi-mode-hook))

(defun qi-get-old-input ()
  "Return a string containing the sexp ending at point."
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (point) end))))

(defun qi-input-filter (str)
  "t if STR does not match `inferior-qi-filter-regexp'."
  (not (string-match inferior-qi-filter-regexp str)))

;;;###autoload
(defun inferior-qi (cmd)
  "Run an inferior Qi process, input and output via buffer `*inferior-qi*'.
If there is a process already running in `*inferior-qi*', just switch
to that buffer.
With argument, allows you to edit the command line (default is value
of `inferior-qi-program').  Runs the hooks from
`inferior-qi-mode-hook' (after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (interactive (list (if current-prefix-arg
			 (read-string "Run qi: " inferior-qi-program)
		       inferior-qi-program)))
  (if (not (comint-check-proc "*inferior-qi*"))
      (let ((cmdlist (split-string cmd)))
	(set-buffer (apply (function make-comint)
			   "inferior-qi" (car cmdlist) nil (cdr cmdlist)))
	(inferior-qi-mode)))
  (setq inferior-qi-buffer "*inferior-qi*")
  (pop-to-buffer "*inferior-qi*"))
;;;###autoload (add-hook 'same-window-buffer-names "*inferior-qi*")

;;;###autoload
(defalias 'run-qi 'inferior-qi)

(defun qi-eval-region (start end &optional and-go)
  "Send the current region to the inferior Qi process.
Prefix argument means switch to the Qi buffer afterwards."
  (interactive "r\nP")
  (comint-send-region (inferior-qi-proc) start end)
  (comint-send-string (inferior-qi-proc) "\n")
  (if and-go (switch-to-qi t)))

(defun qi-eval-defun (&optional and-go)
  "Send the current defun to the inferior Qi process.
Prefix argument means switch to the Qi buffer afterwards."
  (interactive "P")
  (save-excursion
    (end-of-defun)
    (skip-chars-backward " \t\n\r\f") ;  Makes allegro happy
    (let ((end (point)))
      (beginning-of-defun)
      (qi-eval-region (point) end)))
  (if and-go (switch-to-qi t)))

(defun qi-eval-last-sexp (&optional and-go)
  "Send the previous sexp to the inferior Qi process.
Prefix argument means switch to the Qi buffer afterwards."
  (interactive "P")
  (qi-eval-region (save-excursion (backward-sexp) (point)) (point) and-go))

;;; Common Qi COMPILE sux.
(defun qi-compile-region (start end &optional and-go)
  "Compile the current region in the inferior Qi process.
Prefix argument means switch to the Qi buffer afterwards."
  (interactive "r\nP")
  (comint-send-string
   (inferior-qi-proc)
   (format "(funcall (compile nil `(lambda () (progn 'compile %s))))\n"
	   (buffer-substring start end)))
  (if and-go (switch-to-qi t)))

(defun qi-compile-defun (&optional and-go)
  "Compile the current defun in the inferior Qi process.
Prefix argument means switch to the Qi buffer afterwards."
  (interactive "P")
  (save-excursion
    (end-of-defun)
    (skip-chars-backward " \t\n\r\f") ;  Makes allegro happy
    (let ((e (point)))
      (beginning-of-defun)
      (qi-compile-region (point) e)))
  (if and-go (switch-to-qi t)))

(defun switch-to-qi (eob-p)
  "Switch to the inferior Qi process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (if (get-buffer-process inferior-qi-buffer)
      (let ((pop-up-frames
	     ;; Be willing to use another frame
	     ;; that already has the window in it.
	     (or pop-up-frames
		 (get-buffer-window inferior-qi-buffer t))))
	(pop-to-buffer inferior-qi-buffer))
      (run-qi inferior-qi-program))
  (when eob-p
	 (push-mark)
    (goto-char (point-max))))


;;; Now that qi-compile/eval-defun/region takes an optional prefix arg,
;;; these commands are redundant. But they are kept around for the user
;;; to bind if he wishes, for backwards functionality, and because it's
;;; easier to type C-c e than C-u C-c C-e.

(defun qi-eval-region-and-go (start end)
  "Send the current region to the inferior Qi, and switch to its buffer."
  (interactive "r")
  (qi-eval-region start end t))

(defun qi-eval-defun-and-go ()
  "Send the current defun to the inferior Qi, and switch to its buffer."
  (interactive)
  (qi-eval-defun t))

(defun qi-compile-region-and-go (start end)
  "Compile the current region in the inferior Qi, and switch to its buffer."
  (interactive "r")
  (qi-compile-region start end t))

(defun qi-compile-defun-and-go ()
  "Compile the current defun in the inferior Qi, and switch to its buffer."
  (interactive)
  (qi-compile-defun t))

;;; A version of the form in H. Shevis' soar-mode.el package. Less robust.
;;; (defun qi-compile-sexp (start end)
;;;   "Compile the s-expression bounded by START and END in the inferior qi.
;;; If the sexp isn't a DEFUN form, it is evaluated instead."
;;;   (cond ((looking-at "(defun\\s +")
;;; 	 (goto-char (match-end 0))
;;; 	 (let ((name-start (point)))
;;; 	   (forward-sexp 1)
;;; 	   (process-send-string "inferior-qi"
;;; 				(format "(compile '%s #'(lambda "
;;; 					(buffer-substring name-start
;;; 							  (point)))))
;;; 	 (let ((body-start (point)))
;;; 	   (goto-char start) (forward-sexp 1) ; Can't use end-of-defun.
;;; 	   (process-send-region "inferior-qi"
;;; 				(buffer-substring body-start (point))))
;;; 	 (process-send-string "inferior-qi" ")\n"))
;;; 	(t (qi-eval-region start end)))))
;;;
;;; (defun qi-compile-region (start end)
;;;   "Each s-expression in the current region is compiled (if a DEFUN)
;;; or evaluated (if not) in the inferior qi."
;;;   (interactive "r")
;;;   (save-excursion
;;;     (goto-char start) (end-of-defun) (beginning-of-defun) ; error check
;;;     (if (< (point) start) (error "region begins in middle of defun"))
;;;     (goto-char start)
;;;     (let ((s start))
;;;       (end-of-defun)
;;;       (while (<= (point) end) ; Zip through
;;; 	(qi-compile-sexp s (point)) ; compiling up defun-sized chunks.
;;; 	(setq s (point))
;;; 	(end-of-defun))
;;;       (if (< s end) (qi-compile-sexp s end)))))
;;;
;;; End of HS-style code


(defvar qi-prev-l/c-dir/file nil
  "Record last directory and file used in loading or compiling.
This holds a cons cell of the form `(DIRECTORY . FILE)'
describing the last `qi-load-file' or `qi-compile-file' command.")

(defvar qi-source-modes '(qi-mode)
  "*Used to determine if a buffer contains Qi source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a Qi source file by `qi-load-file' and `qi-compile-file'.
Used by these commands to determine defaults.")

(defun qi-load-file (file-name)
  "Load a Qi file into the inferior Qi process."
  (interactive (comint-get-source "Load Qi file: " qi-prev-l/c-dir/file
				  qi-source-modes nil)) ; NIL because LOAD
					; doesn't need an exact name
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq qi-prev-l/c-dir/file (cons (file-name-directory    file-name)
				     (file-name-nondirectory file-name)))
  (comint-send-string (inferior-qi-proc)
		      (format inferior-qi-load-command file-name))
  (switch-to-qi t))


(defun qi-compile-file (file-name)
  "Compile a Qi file in the inferior Qi process."
  (interactive (comint-get-source "Compile Qi file: " qi-prev-l/c-dir/file
				  qi-source-modes nil)) ; NIL = don't need
					; suffix .qi
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq qi-prev-l/c-dir/file (cons (file-name-directory    file-name)
				     (file-name-nondirectory file-name)))
  (comint-send-string (inferior-qi-proc) (concat "(compile-file \""
						   file-name
						   "\"\)\n"))
  (switch-to-qi t))



;;; Documentation functions: function doc, var doc, arglist, and
;;; describe symbol.
;;; ===========================================================================

;;; Command strings
;;; ===============

(defvar qi-function-doc-command
  "(let ((fn '%s))
     (format t \"Documentation for ~a:~&~a\"
	     fn (documentation fn 'function))
     (values))\n"
  "Command to query inferior Qi for a function's documentation.")

(defvar qi-var-doc-command
  "(let ((v '%s))
     (format t \"Documentation for ~a:~&~a\"
	     v (documentation v 'variable))
     (values))\n"
  "Command to query inferior Qi for a variable's documentation.")

(defvar qi-arglist-command
  "(let ((fn '%s))
     (format t \"Arglist for ~a: ~a\" fn (arglist fn))
     (values))\n"
  "Command to query inferior Qi for a function's arglist.")

(defvar qi-describe-sym-command
  "(describe '%s)\n"
  "Command to query inferior Qi for a variable's documentation.")


;;; Ancillary functions
;;; ===================

;;; Reads a string from the user.
(defun qi-symprompt (prompt default)
  (list (let* ((prompt (if default
			   (format "%s (default %s): " prompt default)
			 (concat prompt ": ")))
	       (ans (read-string prompt)))
	  (if (zerop (length ans)) default ans))))


;;; Adapted from function-called-at-point in help.el.
(defun qi-fn-called-at-pt ()
  "Returns the name of the function called in the current call.
The value is nil if it can't find one."
  (condition-case nil
      (save-excursion
	(save-restriction
	  (narrow-to-region (max (point-min) (- (point) 1000)) (point-max))
	  (backward-up-list 1)
	  (forward-char 1)
	  (let ((obj (read (current-buffer))))
	    (and (symbolp obj) obj))))
    (error nil)))


;;; Adapted from variable-at-point in help.el.
(defun qi-var-at-pt ()
  (condition-case ()
      (save-excursion
	(forward-sexp -1)
	(skip-chars-forward "'")
	(let ((obj (read (current-buffer))))
	  (and (symbolp obj) obj)))
    (error nil)))


;;; Documentation functions: fn and var doc, arglist, and symbol describe.
;;; ======================================================================

(defun qi-show-function-documentation (fn)
  "Send a command to the inferior Qi to give documentation for function FN.
See variable `qi-function-doc-command'."
  (interactive (qi-symprompt "Function doc" (qi-fn-called-at-pt)))
  (comint-proc-query (inferior-qi-proc)
		     (format qi-function-doc-command fn)))

(defun qi-show-variable-documentation (var)
  "Send a command to the inferior Qi to give documentation for function FN.
See variable `qi-var-doc-command'."
  (interactive (qi-symprompt "Variable doc" (qi-var-at-pt)))
  (comint-proc-query (inferior-qi-proc) (format qi-var-doc-command var)))

(defun qi-show-arglist (fn)
  "Send a query to the inferior Qi for the arglist for function FN.
See variable `qi-arglist-command'."
  (interactive (qi-symprompt "Arglist" (qi-fn-called-at-pt)))
  (comint-proc-query (inferior-qi-proc) (format qi-arglist-command fn)))

(defun qi-describe-sym (sym)
  "Send a command to the inferior Qi to describe symbol SYM.
See variable `qi-describe-sym-command'."
  (interactive (qi-symprompt "Describe" (qi-var-at-pt)))
  (comint-proc-query (inferior-qi-proc)
		     (format qi-describe-sym-command sym)))


;;  "Returns the current inferior Qi process.
;; See variable `inferior-qi-buffer'."
(defun inferior-qi-proc ()
  (let ((proc (get-buffer-process (if (eq major-mode 'inferior-qi-mode)
				      (current-buffer)
				    inferior-qi-buffer))))
    (or proc
	(error "No Qi subprocess; see variable `inferior-qi-buffer'"))))


;;; Do the user's customisation...
;;;===============================
(defvar inferior-qi-load-hook nil
  "This hook is run when the library `inf-qi' is loaded.
This is a good place to put keybindings.")

(run-hooks 'inferior-qi-load-hook)

;;; CHANGE LOG
;;; ===========================================================================
;;; 2007-5-28
;;;   - added new inferior-qi-mode
;;;   - added error message highlighting
;;;   - changed from using inferior-lisp-mode to using
;;;     new inferior-qi-mode



(provide 'inf-qi)

