(require 'avy)
(require 'anzu)
(require 'browse-kill-ring)
(require 'expand-region)
(require 'unfill)
(require 'diff-hl)
(require 'hungry-delete)
(require 'highlight-escape-sequences)
(require 'image-file)
(require 'vlf)
(require 'goto-line-preview)
(require 'symbol-overlay)
(require 'multiple-cursors)
(require 'expand-region)
(require 'rainbow-delimiters)
(require 'move-dup)
(require 'whole-line-or-region)
(require 'diminish)

(electric-pair-mode 1)
(global-anzu-mode)
(auto-image-file-mode 1)
(cua-selection-mode t)                  ; for rectangles, CUA is nice

(setq avy-background t)
(setq avy-style 'at-full)
(setq-default hungry-delete-chars-to-skip " \t\f\v")
(setq browse-kill-ring-separator "\f")
(setq show-paren-style 'mixed)

;; Don't disable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Don't disable case-change  functions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)




(add-hook 'prog-mode-hook 'diff-hl-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
(add-hook 'after-init-hook 'transient-mark-mode)
(add-hook 'after-init-hook 'global-hungry-delete-mode)
(add-hook 'after-init-hook 'show-paren-mode)
(add-hook 'after-init-hook 'hes-mode)

(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

(when (fboundp 'global-prettify-symbols-mode)
  (add-hook 'after-init-hook 'global-prettify-symbols-mode))

(add-hook 'after-init-hook 'whole-line-or-region-global-mode)


(global-set-key [remap goto-line] 'goto-line-preview)
(after-load 'page-break-lines
  (push 'browse-kill-ring-mode page-break-lines-modes))



(diminish 'anzu-mode)
(diminish 'hungry-delete-mode)
(after-load 'autorevert
  (diminish 'auto-revert-mode))
(after-load 'subword
  (diminish 'subword-mode))
(diminish 'symbol-overlay-mode)
(after-load 'page-break-lines
  (diminish 'page-break-lines-mode))
(after-load 'whole-line-or-region
  (diminish 'whole-line-or-region-local-mode))



(dolist (hook (list
               'prog-mode-hook
               'html-mode-hook
               'yaml-mode-hook
               'conf-mode-hook))
  (add-hook hook '(lambda ()
                    (symbol-overlay-mode))))



(defun ffap-vlf ()
  "Find file at point with VLF."
  (interactive)
  (let ((file (ffap-file-at-point)))
    (unless (file-exists-p file)
      (error "File does not exist: %s" file))
    (vlf file)))

(global-set-key (kbd "RET") 'newline-and-indent)
(defun lew/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(when (fboundp 'display-line-numbers-mode)
    (defun lew/with-display-line-numbers (f &rest args)
      (let ((display-line-numbers t))
        (apply f args)))
    (advice-add 'goto-line-preview :around #'lew/with-display-line-numbers))

(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)

(defun lew/backward-up-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (lew/backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'lew/backward-up-sexp) ; C-M-u, C-M-up

(defun lew/open-line-with-reindent (n)
  "A version of `open-line' which reindents the start and end positions.
If there is a fill prefix and/or a `left-margin', insert them
on the new line if the line would have been blank.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
         (do-left-margin (and (bolp) (> (current-left-margin) 0)))
         (loc (point-marker))
         ;; Don't expand an abbrev before point.
         (abbrev-mode nil))
    (delete-horizontal-space t)
    (newline n)
    (indent-according-to-mode)
    (when (eolp)
      (delete-horizontal-space t))
    (goto-char loc)
    (while (> n 0)
      (cond ((bolp)
             (if do-left-margin (indent-to (current-left-margin)))
             (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq n (1- n)))
    (goto-char loc)
    (end-of-line)
    (indent-according-to-mode)))

(global-set-key (kbd "C-o") 'lew/open-line-with-reindent)

;; Duplicate start of line or region with C-M-<end>.
;; From http://www.emacswiki.org/emacs/DuplicateStartOfLineOrRegion
(defun duplicate-start-of-line-or-region ()
  (interactive)
  (if mark-active
      (duplicate-region)
    (duplicate-start-of-line)))
(defun duplicate-start-of-line ()
  (if (bolp)
      (progn
        (end-of-line)
        (duplicate-start-of-line)
        (beginning-of-line))
    (let ((text (buffer-substring (point)
                                  (beginning-of-thing 'line))))
      (forward-line)
      (push-mark)
      (insert text)
      (open-line 1))))
(defun duplicate-region ()
  (let* ((end (region-end))
         (text (buffer-substring (region-beginning) end)))
    (goto-char end)
    (insert text)
    (push-mark end)
    (setq deactivate-mark nil)
    (exchange-point-and-mark)))

(global-set-key (kbd "C-M-<end>") 'duplicate-start-of-line-or-region)


(defun lew/disable-features-during-macro-call (orig &rest args)
  "When running a macro, disable features that might be expensive.
ORIG is the advised function, which is called with its ARGS."
  (let (post-command-hook
        font-lock-mode
        (tab-always-indent (or (eq 'complete tab-always-indent) tab-always-indent)))
    (apply orig args)))

(advice-add 'kmacro-call-macro :around 'lew/disable-features-during-macro-call)

;; Hack for setting a fixed wrap column in visual-line-mode.
(defun lew/set-visual-wrap-column (new-wrap-column &optional buffer)
  "Force visual line wrap at NEW-WRAP-COLUMN in BUFFER (defaults
    to current buffer) by setting the right-hand margin on every
    window that displays BUFFER.  A value of NIL or 0 for
    NEW-WRAP-COLUMN disables this behavior."
  (interactive (list (read-number "New visual wrap column, 0 to disable: " (or visual-wrap-column fill-column 0))))
  (if (and (numberp new-wrap-column)
           (zerop new-wrap-column))
      (setq new-wrap-column nil))
  (with-current-buffer (or buffer (current-buffer))
    (visual-line-mode t)
    (set (make-local-variable 'visual-wrap-column) new-wrap-column)
    (add-hook 'window-configuration-change-hook 'update-visual-wrap-column nil t)
    (let ((windows (get-buffer-window-list)))
      (while windows
        (when (window-live-p (car windows))
          (with-selected-window (car windows)
            (update-visual-wrap-column)))
        (setq windows (cdr windows))))))

(defun lew/update-visual-wrap-column ()
  (if (not visual-wrap-column)
      (set-window-margins nil nil)
    (let* ((current-margins (window-margins))
           (right-margin (or (cdr current-margins) 0))
           (current-width (window-width))
           (current-available (+ current-width right-margin)))
      (if (<= current-available visual-wrap-column)
          (set-window-margins nil (car current-margins))
        (set-window-margins nil (car current-margins)
                            (- current-available visual-wrap-column))))))

;; A function for easily editing a file as root through TRAMP.
(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (if (fboundp 'helm-read-file-name)
                             (helm-read-file-name "File: ")
                           (ido-read-file-name "File: "))))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))


(defun lew/css-expand-statement ()
  (interactive)
  (save-excursion
    (end-of-line)
    (search-backward "{")
    (forward-char 1)
    (let ((beg (point)))
      (newline)
      (er/mark-inside-pairs)
      (replace-regexp ";" ";\n" nil (region-beginning) (region-end))
      (indent-region beg (point)))))

(defun lew/css-contract-statement ()
  (interactive)
  (end-of-line)
  (search-backward "{")
  (while (not (looking-at "}"))
    (join-line -1))
  (back-to-indentation))


(provide 'init-editing)
