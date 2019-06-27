(require 'insert-translated-name)

(dolist (hook (list
               'org-mode-hook
               'markdown-mode-hook
               'prog-mode-hook
               'text-mode-hook
               ))
  (add-hook hook '(lambda ()
                    (insert-translated-name-use-original-translation))
            ))

(provide 'init-insert-translated-name)
