;; after-load
  (if (fboundp 'with-eval-after-load)
      (defalias 'after-load 'with-eval-after-load)
    (defmacro after-load (feature &rest body)
      "After FEATURE is loaded, evaluate BODY."
      (declare (indent defun))
      `(eval-after-load ,feature
         '(progn ,@body))))

;; add-auto-mode
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

;; multiple commands
(defmacro advise-commands (advice-name commands &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS.
The body of the advice is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice ,command
                    (before ,(intern
                              (concat
                               (symbol-name command)
                               "-"
                               advice-name))
                            activate)
                    ,@body))
               commands)))

;; https://github.com/technomancy/emacs-starter-kit/blob/master/starter-kit-defuns.el
(defun add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

(add-hook 'coding-hook 'add-watchwords)

(defun run-coding-hook ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks 'coding-hook))

;;https://github.com/marktran/emacs.d/blob/master/definitions/set-key.el
(defun set-key (map spec cmd)
  "Set in `map' `spec' to `cmd'.
`Map' may be `'global' `'local' or a keymap.
A `spec' can be a `read-kbd-macro'-readable string or a vector."
  (let ((setter-fun (case map
                      (global #'global-set-key)
                      (local  #'local-set-key)
                      (t      (lambda (key def) (define-key map key def)))))
        (key (typecase spec
               (vector spec)
               (string (read-kbd-macro spec))
               (t (error "wrong argument")))))
    (funcall setter-fun key cmd)))




(provide 'basic-utils)
