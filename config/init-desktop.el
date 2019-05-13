
;; 恢复桌面，启动时间需要衡量

(require 'session)

(setq session-save-file (expand-file-name ".session" jakelew-emacs-root-dir))
(setq session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\)")
(setq session-save-file-coding-system 'utf-8)

(add-hook 'after-init-hook 'session-initialize)




(setq desktop-path (list jakelew-emacs-root-dir)
      desktop-auto-save-timeout 600)
(desktop-save-mode 1)


(provide 'init-desktop)
