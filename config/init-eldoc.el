(dolist (hook (list
               'ielm-mode-hook
               'emacs-lisp-mode-hook
               'lisp-interaction-mode-hook
               'message-mode-hook
               'org-mode-hook))
  (add-hook hook '(lambda ()
                    (progn
                      (require 'eldoc)
                      (setq eldoc-idle-delay 0)
                      (turn-on-eldoc-mode)))))

(diminish 'eldoc-mode)

(provide 'init-eldoc)
