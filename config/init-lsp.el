
(require 'lsp-mode)

;; lsp支持特定的mode, 而不是全局
(dolist (hook (list
               'js-mode-hook
               'ruby-mode-hook
               'go-mode-hook
               ))
  (add-hook hook '(lambda ()
                    (lsp)
                    )))

(provide 'init-lsp)
