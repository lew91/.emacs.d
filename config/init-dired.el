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


(with-eval-after-load 'dired
  ;; Hydra mapping for dired taken from https://github.com/abo-abo/hydra/wiki/Dired
  (defhydra hydra-dired (:hint nil :color pink)
    "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
    ("\\" dired-do-ispell)
    ("(" dired-hide-details-mode)
    (")" dired-omit-mode)
    ("+" dired-create-directory)
    ("=" diredp-ediff)         ;; smart diff
    ("?" dired-summary)
    ("$" diredp-hide-subdir-nomove)
    ("A" dired-do-find-regexp)
    ("C" dired-do-copy)        ;; Copy all marked files
    ("D" dired-do-delete)
    ("E" dired-mark-extension)
    ("e" dired-ediff-files)
    ("F" dired-do-find-marked-files)
    ("G" dired-do-chgrp)
    ("g" revert-buffer)        ;; read all directories again (refresh)
    ("i" dired-maybe-insert-subdir)
    ("l" dired-do-redisplay)   ;; relist the marked or singel directory
    ("M" dired-do-chmod)
    ("m" dired-mark)
    ("O" dired-display-file)
    ("o" dired-find-file-other-window)
    ("Q" dired-do-find-regexp-and-replace)
    ("R" dired-do-rename)
    ("r" dired-do-rsynch)
    ("S" dired-do-symlink)
    ("s" dired-sort-toggle-or-edit)
    ("t" dired-toggle-marks)
    ("U" dired-unmark-all-marks)
    ("u" dired-unmark)
    ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
    ("w" dired-kill-subdir)
    ("Y" dired-do-relsymlink)
    ("z" diredp-compress-this-file)
    ("Z" dired-do-compress)
    ("q" nil)
    ("." nil :color blue))

  (define-key dired-mode-map "." 'hydra-dired/body))

(provide 'init-dired)
;; init-dired.el ends here
