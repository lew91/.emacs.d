;;; init-slime.el --- Slime for common lisp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'slime)
(require 'slime-company)
(require 'hippie-expand-slime)

;; auto-complete
(after-load 'slime
  (slime-setup '(slime-company)))

;; package.el compiles the contrib subdir, but the compilation order
;; causes problems, so we remove the .elc files there. See
;; http://lists.common-lisp.net/pipermail/slime-devel/2012-February/018470.html
;;(mapc #'delete-file
;;      (file-expand-wildcards (concat user-emacs-directory "elpa/slime-2*/contrib/*.elc")))

(defun lew/slime-setup ()
  "Mode setup function for slime lisp buffers."
  (set-up-slime-hippie-expand))

(after-load 'slime
  (setq slime-protocol-version 'ignore)
  (setq slime-net-coding-system 'utf-8-unix)
  (let ((extras (when (require 'slime-company nil t)
                  '(slime-company))))
    (slime-setup (append '(slime-repl slime-fuzzy) extras)))
  (setq slime-complete-symbol*-fancy t)
  (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
  (add-hook 'slime-mode-hook 'lew/slime-setup))

(defun lew/slime-repl-setup ()
  "Mode setup function for slime REPL."
  (enable-paredit-mode)
  (set-up-slime-hippie-expand))

(after-load 'slime-repl
  ;; Stop SLIME's REPL from grabbing DEL, which is annoying when backspacing over a '('
  (after-load 'paredit
    (define-key slime-repl-mode-map (read-kbd-macro paredit-backward-delete-key) nil))

  ;; Bind TAB to `indent-for-tab-command', as in regular Slime buffers.
  (define-key slime-repl-mode-map (kbd "TAB") 'indent-for-tab-command)

  (add-hook 'slime-repl-mode-hook 'lew/slime-repl-setup))

(defun lew/slime-eval-sexp-of-line ()
  "Evaluate current line."
  (interactive)
  (move-end-of-line 1)
  (slime-eval-last-expression))

(defun lew/cl-eval-current-form ()
  "Find and evaluate the current def* or set* command."
  (interactive)
  (save-excursion
    (search-backward-regexp "(def\\|(set")
    (forward-list)
    (call-interactively 'slime-eval-last-expression)))


(provide 'init-slime)
;;; init-slime.el ends here
