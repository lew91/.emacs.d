;;(require 'dired+)

(setq dired-recursive-copies t)    ;可以递归的进行拷贝
(setq dired-recursive-deletes t)   ;可以递归的删除目录
(setq dired-recursive-deletes 'always)  ;删除东西时不提示
(setq dired-recursive-copies 'always) ;拷贝东西时不提示

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

(let ((gls "/usr/local/bin/gls"))       ; 因为使用了'cache-path-from-shell.el'，全局初始化了一次'exec-path-from-shell'。这里设置成从绝对路径调用'gls'
  (if (file-exists-p gls)
      (setq insert-directory-program gls)))

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


(require 'diff-hl)
(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

;; (with-eval-after-load 'dired+
;;   ;; show the details when using dired+
;;   (setq diredp-hide-details-propagate-flag nil)
;;   (setq diredp-hide-details-initially-flag nil)

;;   ;; form https://www.emacswiki.org/emacs/DiredPlus
;;   (setq directory-listing-before-filename-regexp
;;         (let* ((l "\\([A-Za-z]\\|[^\0-\177]\\)")
;;                (l-or-quote "\\([A-Za-z']\\|[^\0-\177]\\)")
;;                (month (concat l-or-quote l-or-quote "+\\.?"))
;;                (s " ")
;;                (yyyy "[0-9][0-9][0-9][0-9]")
;;                (dd "[ 0-3][0-9]")
;;                (HH:MM "[ 0-2][0-9][:.][0-5][0-9]")
;;                (seconds "[0-6][0-9]\\([.,][0-9]+\\)?")
;;                (zone "[-+][0-2][0-9][0-5][0-9]")
;;                (iso-mm-dd "[01][0-9]-[0-3][0-9]")
;;                (iso-time (concat HH:MM "\\(:" seconds "\\( ?" zone "\\)?\\)?"))
;;                (iso (concat "\\(\\(" yyyy "-\\)?" iso-mm-dd "[ T]" iso-time
;;                             "\\|" yyyy "-" iso-mm-dd "\\)"))
;;                (western (concat "\\(" month s "+" dd "\\|" dd "\\.?" s month "\\)"
;;                                 s "+"
;;                                 "\\(" HH:MM "\\|" yyyy "\\)"))
;;                (western-comma (concat month s "+" dd "," s "+" yyyy))
;;                (mm "[ 0-1]?[0-9]")
;;                (east-asian
;;                 (concat "\\(" mm l "?" s dd l "?" s "+"
;;                         "\\|" dd s mm s "+" "\\)"
;;                         "\\(" HH:MM "\\|" yyyy l "?" "\\)")))
;;           (purecopy (concat ".*[0-9][BkKMGTPEZY]?" s
;;                             "\\(" western "\\|" western-comma "\\|" east-asian "\\|" iso "\\)"
;;                             s "+")))))



(provide 'init-dired)
;; init-dired.el ends here
