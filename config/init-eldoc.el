(dolist (hook (list
               'ielm-mode-hook
               'emacs-lisp-mode-hook
               'lisp-interaction-mode-hook
               'message-mode-hook
               'org-mode-hook))
  (add-hook hook '(lambda ()
                    (progn
                      (require 'eldoc)
                      (require 'eldoc-extension)
                      (setq eldoc-idle-delay 0)
                      (setq eldoc-argumhent-case 'eldoc-argument-list)
                      (turn-on-eldoc-mode)))))

(provide 'init-eldoc)
