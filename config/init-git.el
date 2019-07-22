(require 'magit)


(with-eval-after-load 'magit
  ;; Magit configuration.
  (setq magit-commit-ask-to-stage nil)    ;don't ask stage question
  (setq magit-display-buffer-noselect t) ;don't select magit buffer default

  ;; Make path column have enough space to display.
  (setq magit-submodule-list-columns
        '(("Path"     80 magit-modulelist-column-path   nil)
          ("Version"  30 magit-repolist-column-version  nil)
          ("Branch"   20 magit-repolist-column-branch   nil)
          ("B<U" 3 magit-repolist-column-unpulled-from-upstream   ((:right-align t)))
          ("B>U" 3 magit-repolist-column-unpushed-to-upstream     ((:right-align t)))
          ("B<P" 3 magit-repolist-column-unpulled-from-pushremote ((:right-align t)))
          ("B>P" 3 magit-repolist-column-unpushed-to-pushremote   ((:right-align t)))
          ("B"   3 magit-repolist-column-branches                 ((:right-align t)))
          ("S"   3 magit-repolist-column-stashes                  ((:right-align t)))))


  (setq-default magit-diff-refine-hunk t)
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch)

  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'git-commit-mode-hook 'goto-address-mode)
)


(defun jl/magit-or-vc-log-file (&optional prompt)
  (interactive "P")
  (if (and (buffer-file-name)
           (eq 'Git (vc-backend (buffer-file-name))))
      (if prompt
          (magit-log-buffer-file-popup)
        (magit-log-buffer-file t))
    (vc-print-log)))

(with-eval-after-load 'vc
  (define-key vc-prefix-map (kbd "l") 'jl/magit-or-vc-log-file))


;; String utilities missing from core emacs
(defun jl/string-all-matches (regex str &optional group)
  "Find all matches for 'REGEX' within 'STR', retturing the full match string or group 'GROUP'."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))


(defvar git-svn--available-commands nil "Cached list of git svn subcommands")
(defun git-svn--available-commands ()
  (or git-svn--available-commands
      (setq git-svn--available-commands
            (jl/string-all-matches
             "^  \\([a-z\\-]+\\) +"
             (shell-command-to-string "git svn help") 1))))

(autoload 'vc-git-root "vc-git")

(defun git-svn (dir command)
  "Run a git svn subcommand in DIR."
  (interactive (list (read-directory-name "Directory: ")
                     (completing-read "git-svn command: " (git-svn--available-commands) nil t nil nil (git-svn--available-commands))))
  (let* ((default-directory (vc-git-root dir))
         (compilation-buffer-name-function (lambda (major-mode-name) "*git-svn*")))
    (compile (concat "git svn " command))))



(defun jl/magit-submodule-add+ (url)
  (interactive "sURL: ")
  (let ((parent-dir (cadr (split-string (file-name-as-directory jl-emacs-extension-dir) (expand-file-name (cdr (project-current)))))))
    (magit-submodule-add
     url
     (concat parent-dir (file-name-base url))
     (file-name-base url))))


(defun jl/magit-submodule-remove+ ()
  (interactive)
  (magit-submodule-remove (list (magit-read-module-path "Remove module")) "--force" nil))


(defun jl/magit-status+ ()
  (interactive)
  (magit-status)
  (other-window 1))


(defun jl/magit-blame+ ()
  (interactive)
  (setq magit-blame--style
        '(margin
          (margin-format " %s%f" " %C %a" " %H")
          (margin-width . 42)
          (margin-face . magit-blame-margin)
          (margin-body-face magit-blame-dimmed)))
  (magit-blame))


(defun jl/magit-delete-remote-branch ()
  (interactive)
  (when (y-or-n-p (format "Delete remote branch (%s): " (magit-get-current-branch)))
    (magit-run-git-async "push" "origin" (format ":%s" (magit-get-current-branch)))))


(provide 'init-git)
