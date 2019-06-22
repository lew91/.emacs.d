
(require 'lsp-mode)
(require 'lsp-clients)
(require 'lsp-ui)
(require 'company-lsp)

(setq ;;lsp-auto-guess-root t             ; Detect project root
      lsp-prefer-flymake nil            ; Use lsp-ui and flycheck
      flymake-fringe-indicator-position 'right-fringe)

;; clangd
(when (equal system-type 'darwin)
  (setq lsp-clients-clangd-executable "/usr/local/opt/llvm/bin/clangd"))

(setq lsp-ui-doc-header t
      lsp-ui-doc-include-signature t
      lsp-ui-doc-enable t
      lsp-ui-doc-delay 1.0
      lsp-ui-doc-postition 'at-point)

(push 'company-lsp company-backends)

;; lsp支持特定的mode, 而不是全局
(dolist (hook (list
               'js-mode-hook
               'ruby-mode-hook
               'go-mode-hook
               'c-mode-hook
               'c++-mode-hook
               ))
  (add-hook hook '(lambda ()
                    (lsp)
                    )))
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'lsp-after-open-hook 'lsp-enable-imenu)

;;(define-key lsp-mode-map (kbd "C-c C-d") 'lsp-describe-thing-at-point)
(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

;; (defun /lsp/suggest-project-root ()
;;   "suggest the nearest project that is not a dependency."
;;   (or
;;    (locate-dominating-file
;;     (buffer-file-name)
;;     (lambda (dir)
;;       (if (string-match-p "node_modules" dir)
;;           nil
;;         (file-exists-p (concat dir "package.json")))))
;;    (projectile-project-root)))


;; (advice-add #'lsp--suggest-project-root :override #'/lsp/suggest-project-root)


(provide 'init-lsp)
