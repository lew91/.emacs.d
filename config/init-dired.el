(require 'dired-x)
(require 'dash)

(setq dired-recursive-copies t)
(setq dired-recursive-deletes t)
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

(setq dired-auto-revert-buffer t  ; Revert on re-visiting
      dired-listing-switches "-alhF" ; better dired flags: '-l' is mandatory, '-a' shows all files, '-h' uses human-readable sizes, and '-F' appends file-type classifiers to file names ( for better highlighting)
      dired-dwim-target t   ; auto-copy to other Dired split window
      dired-ls-F-marks-symlinks t  ; -F marks links with @
      )

(setq delete-by-moving-to-trash t)

(defun system-move-file-to-trash (file)
  "Use \"trash\" to move FILE to the system trash.
When using Homebrew, install it using \"brew install trash\"."
  (call-process (executable-find "trash")
		nil 0 nil
		file))


(when (or (memq system-type '(gnu gnu/linux))
          (string= (file-name-nondirectory insert-directory-program) "gls"))
  ;; If we are on a GNU system or have GNU ls, add some more `ls' switches:
  ;; `--group-directories-first' lists directories before files, and `-v'
  ;; sorts numbers in file names naturally, i.e. "image1" goes before
  ;; "image02"
  (setq dired-listing-switches
        (concat dired-listing-switches " --group-directories-first -v")))


;; (let ((gls "/usr/local/bin/gls"))     
;;   (if (file-exists-p gls)
;;       (setq insert-directory-program gls)))

(when (executable-find "gls")
  ;; Use GNU ls as 'gls' from 'coreutils' if available.
  (setq insert-directory-program "gls"))


(defun dired-jump-kill-buffer (&rest)
  (interactive)
  (let ((buf (current-buffer)))
    (dired-jump)
    (kill-buffer buf)))


(with-eval-after-load  'dired
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (define-key dired-mode-map (kbd "C-c C-q") 'wdired-change-to-wdired-mode)
  )


(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode))


;; Reload dired after making changes
(--each '(dired-do-rename
          dired-do-copy
          dired-create-directory
          dired-abort-changes
          (eval `(defadvice ,it (after revert-buffer activate)
                   (revert-buffer)))))


;; Hook up dired-x global bindings without loading it up-front
(define-key ctl-x-map "\C-j" 'dired-jump)
(define-key ctl-x-4-map "\C-j" 'dired-jump-other-window)


;; In Mac, use finder open file
(defun finder ()
  "Opens file directory in Finder."
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (shell-command
         (format "%s %s" (executable-find "open") (file-name-directory file)))
      (error "Buffer is not attached to any file."))))


(provide 'init-dired)
;; init-dired.el ends here
