
(require 'lsp-mode)
(require 'lsp-clients)
(require 'lsp-ui)
(require 'company-lsp)

(setq lsp-auto-guess-root nil             ; default is nil, wanna auto indicator? set it t
      lsp-prefer-flymake t              ; Use flycheck set it nil, otherwise t for flymake
      flymake-fringe-indicator-position 'left-fringe)

;; clangd
(when (equal system-type 'darwin)
  (setq lsp-clients-clangd-executable "/usr/local/opt/llvm/bin/clangd"))

(setq lsp-ui-doc-header t
      lsp-ui-doc-include-signature t
      lsp-ui-doc-enable t
      lsp-ui-doc-delay 1.0
      lsp-ui-doc-postition 'at-point)

(after-load 'lsp-mode
  (after-load 'company
    (push 'company-lsp company-backends)))

;; lsp支持特定的mode, 而不是全局
(dolist (hook (list
               'js-mode-hook
               'ruby-mode-hook
               'go-mode-hook
               'c-mode-hook
               'c++-mode-hook
               'python-mode-hook
               ))
  (add-hook hook '(lambda ()
                    (lsp)
                    )))
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'lsp-after-open-hook 'lsp-enable-imenu)

(after-load 'lsp-ui
  ;;(define-key lsp-mode-map (kbd "C-c C-d") 'lsp-describe-thing-at-point)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))



(provide 'init-lsp)
