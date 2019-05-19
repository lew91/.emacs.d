(require 'ycmd)

(set-variable 'ycmd-server-command '("python3" "/users/jakelew/.vim/bundle/YouCompleteMe/third_party/ycmd/ycmd/"))
(set-variable 'ycmd-global-config (expand-file-name "~/.vim/bundle/YouCompleteMe/third_party/ycmd/.ycm_extra_conf.py"))
;;(add-hook 'C++-mode-hook 'ycmd-mode)
;;(add-hook 'after-init-hook 'global-ycmd-mode)

(dolist (hook (list
               'c-mode-hook
               'c++-mode-hook
               )
              )
  (add-hook hook '(lambda()
                    (ycmd-mode)
                    )))




(require 'company-ycmd)
(company-ycmd-setup)
(add-to-list 'company-backends (company-mode/backend-with-yas 'company-ycmd))

(require 'flycheck-ycmd)
(flycheck-ycmd-setup)
(add-hook 'ycmd-mode-hook 'flycheck-ycmd-setup)

(require 'ycmd-eldoc)
(add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup)

(provide 'init-ycmd)
