(require-package 'ycmd)

(set-variable 'ycmd-server-command '("python3" "/users/jakelew/.vim/bundle/YouCompleteMe/third_party/ycmd/ycmd/"))
(set-variable 'ycmd-global-config (expand-file-name "~/.vim/bundle/YouCompleteMe/third_party/ycmd/.ycm_extra_conf.py"))  ;contained in vim directories


(dolist (hook (list
               'c-mode-hook
               'c++-mode-hook
               )
              )
  (add-hook hook '(lambda()
                    (ycmd-mode)
                    )))

(defun ycmd/manual-semantic-company-completer ()
  "A useful function that can be bound, if users prefer to trigger company
completion manually"

  (interactive)
  (company-cancel)
  (let ((ycmd-force-semantic-completion (not (company-ycmd--in-include))))
    (setq company-backend 'company-ycmd)
    (company-manual-begin)))


(require-package 'company-ycmd)
(company-ycmd-setup)
(add-to-list 'company-backends (company-mode/backend-with-yas 'company-ycmd))

(require-package 'flycheck-ycmd)
(flycheck-ycmd-setup)
(add-hook 'ycmd-mode-hook 'flycheck-ycmd-setup)

(maybe-require-package 'ycmd-eldoc)
(add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup)

(provide 'init-ycmd)
