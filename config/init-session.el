
;;; Require
(require 'auto-save)
;;(require 'session)


;;; Code:

;; (setq desktop-path (list jl-emacs-root-dir)
;;       desktop-auto-save-timeout 600)
;; (desktop-save-mode 1)

;; (setq session-save-file (expand-file-name ".session" jl-emacs-root-dir))
;; (setq session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\)")
;; (setq session-save-file-coding-system 'utf-8)

;; (add-hook 'after-init-hook 'session-initialize)

(setq desktop-load-locked-desktop t) ; don't popup dialog ask user, load anyway
(setq desktop-restore-frames nil)    ; 不要保存框架和窗口配置

(defun emacs-session-restore ()
  "Restore emacs session."
  (interactive)
  (ignore-errors
    ;; Kill unused buffers.
    (kill-unused-buffers)
    ;; Restore session.
    (desktop-read "~/.emacs.d/")
    ))

(defun emacs-session-save (arg)
  "Save emacs session."
  (interactive "p")
  (ignore-errors
    (if (equal arg 4)
        ;; Kill all buffers if with prefix argument.
        (mapc 'kill-buffer (buffer-list))
      ;; Kill unused buffers.
      (kill-unused-buffers)
      ;; Save all buffers before exit.
      (auto-save-buffers))
    ;; Save session.
    (make-directory "~/.emacs.d/" t)
    (desktop-save "~/.emacs.d/")
    ;; Exit emacs.
    (kill-emacs)))


(provide 'init-session)

;;; init-session.el ends here
