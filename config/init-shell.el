
(setenv "PATH"
        (concat
         "/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/bin:/Library/TeX/texbin:"
         (getenv "PATH")))
(setenv "EMACS" "/Applications/Emacs.app/Contents/MacOS/Emacs")
(getenv "EMACS")


;; When use zsh, the startup time is too long.
(when (eq system-type 'windows-nt)
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; (with-eval-after-load 'exec-path-from-shell
;;   (dolist (var '("PATH" "SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"))
;;     (add-to-list 'exec-path-from-shell-variables var))
;;   (setq exec-path-from-shell-check-startup-files nil))


;; (when (or (memq window-system '(mac ns x))
;;           (unless (memq system-type '(ms-dos windows-nt))
;;             (daemonp)))
;;   (exec-path-from-shell-initialize))


;;; Use Python3
(setq python-shell-interpreter "python3")
;;(setq python-shell-interpreter-args "-m IPython --simple-prompt -i")
;;(setq flycheck-python-pycompile-executable "python3")          ; if use flycheck



(provide 'init-shell)
;;; init-shell.el ends here
