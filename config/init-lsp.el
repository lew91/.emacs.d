
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

(after-load 'lsp
  (setq lsp-prefer-flymake nil)
  (maybe-require-package 'lsp-ui)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-header t))


(defun /lsp/suggest-project-root ()
  "suggest the nearest project that is not a dependency."
  (or
   (locate-dominating-file
    (buffer-file-name)
    (lambda (dir)
      (if (string-match-p "node_modules" dir)
          nil
        (file-exists-p (concat dir "package.json")))))
   (projectile-project-root)))

(after 'lsp-mode
       (advice-add #'lsp--suggest-project-root :override #'/lsp/suggest-project-root))


(provide 'init-lsp)
