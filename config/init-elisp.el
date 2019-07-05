;;; init-elisp.el  --- Additional functionality for elisp, eldoc

(require 'pp)

(add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "ELisp")))


(setq load-prefer-newer t) ; load .el if newer than corresponding .elc


(defun jl/insert-headerise-elisp ()
  "Add minimal header and footer to an elisp buffer in order to placate flycheck."
  (interactive)
  (let ((fname (if (buffer-file-name)
                   (file-name-nondirectory (buffer-file-name))
                 (error "This buffer is not visiting a file"))))
    (save-excursion
      (goto-char (point-min))
      (insert ";;; " fname " --- Insert description here -*- lexical-binding: t -*-\n"
              ";;; Commentary:\n"
              ";;; Code:\n\n")
      (goto-char (point-max))
      (insert ";;; " fname " ends here\n"))))

(defun jl/eval-last-sexp-or-region (prefix)
  "Eval region from BEG to END if active, otherwise the last sexp."
  (interactive "P")
  (if (and (mark) (use-region-p))
      (eval-region (min (point) (mark)) (max (point) (mark)))
    (pp-eval-last-sexp prefix)))

(global-set-key [remap eval-expression] 'pp-eval-expression)

(with-eval-after-load 'lisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-x C-e") 'jl/eval-last-sexp-or-region))

(defun jl/make-read-only (expression out-buffer-name)
  "Enable `view-mode' in the output buffer - if any - so it can be closed with `\"q\"."
  (when (get-buffer out-buffer-name)
    (with-current-buffer out-buffer-name
      (view-mode 1))))
(advice-add 'pp-display-expression :after 'jl/make-read-only)


(defun jl/maybe-set-bundled-elisp-readonly ()
  "If this elisp appears to be part of Emacs, then disallow editing."
  (when (and (buffer-file-name)
             (string-match-p "\\.el\\.gz\\'" (buffer-file-name)))
    (setq buffer-read-only t)
    (view-mode t)))

(add-hook 'emacs-lisp-mode-hook 'jl/maybe-set-bundled-elisp-readonly)

(defun jl/eval-buffer (arg)
  "Execute the current buffer as Lisp code.
Top-level forms are evaluated with `eval-defun' so that `defvar'
and `defcustom' forms reset their default values."
  (interactive "P")
  (if (equal arg (list 4))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (forward-sexp)
          (eval-defun nil))
        (message "Redefined buffer!"))
    (eval-buffer)))

;; Support byte-compilation in a sub-process, as required by highlight-cl
(defun jl/byte-compile-file-batch (filename)
  "Byte-compile FILENAME in batch mode, ie. a clean sub-process."
  (interactive "fFile to byte-compile in batch mode: ")
  (let ((emacs (car command-line-args)))
    (compile
     (concat
      emacs " "
      (mapconcat
       'shell-quote-argument
       (list "-Q" "-batch" "-f" "batch-byte-compile" filename)
       " ")))))


;;;###autoload
(defun jl/pp-eval-expression (expression)
  "Same as 'pp-eval-expression' but without \"Evaluating..\" message."
  (interactive
   (list (read--expression "Eval: ")))
  (setq values (cons (eval expression) values))
  (pp-display-expression (car values) "*Pp Eval Output*"))


;;;###autoload
(defun jl/eval-dwin (arg)
  "Eval last sexp or region if it is active. ARG is passed to 'eval-last-sexp'."
  (interactive "P")
  (if (use-region-p)
      (eval-region (region-beginning) (region-end))
    (eval-last-sexp arg)))

;;;###autoload
(defun jl/pp-veal-dwin (arg)
  "Eval last sexp or region if it is active. ARG is passed to 'pp-eval-last-sexp'."
  (interactive "P")
  (if (use-region-p)
      (eval-region (region-beginning) (region-end))
    (pp-eval-last-sexp arg)))

;;;###autoload
(defun jl/indent-sexp (&optional no-offset pp)
  "Indent each line of the list starting just after point.
If NO-OFFSET is non-nil (with \\[universal-argument]), indent
without offset for the following lines.
If PP is non-nil (with \\[universal-argument] \\[universal-argument]), pretty-print the following list."
  (interactive
   (list (equal current-prefix-arg '(4))
         (equal current-prefix-arg '(16))))
  (let ((lisp-indent-offset (and no-offset 1)))
    (indent-pp-sexp pp)))

(provide 'init-elisp)
;;; init-elisp.el ends here
