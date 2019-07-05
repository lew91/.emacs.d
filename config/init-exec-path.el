(require 'exec-path-from-shell)

(with-eval-after-load 'exec-path-from-shell
  (dolist (var '("PATH" "SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"))
    (add-to-list 'exec-path-from-shell-variables var)))

(setq exec-path-from-shell-check-startup-files nil)

(when (or (memq window-system '(mac ns x))
          (unless (memq system-type '(ms-dos windows-nt))
            (daemonp)))
  (exec-path-from-shell-initialize))

(provide 'init-exec-path)
;;; init-exec-path.el ends here
