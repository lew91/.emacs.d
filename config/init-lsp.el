(require 'eglot)
(require 'lsp-mode)
(require 'lsp-clients)
(require 'lsp-ui)
(require 'company-lsp)

(add-hook 'python-mode-hook 'eglot-ensure)


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

(with-eval-after-load 'lsp-mode
  (with-eval-after-load 'company
    (push 'company-lsp company-backends)
    (setq company-lsp-enable-snippet t)
    (setq company-lsp-cache-candidates t)))

;; Use lsp-mode only in some special major mode
(dolist (hook (list
               'js-mode-hook
               'ruby-mode-hook
               'go-mode-hook
               'c-mode-hook
               'c++-mode-hook
               ;;'python-mode-hook
               ))
  (add-hook hook '(lambda ()
                    (lsp)
                    )))
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'lsp-after-open-hook 'lsp-enable-imenu)

(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-h D")  'eglot-help-at-point)) 

(with-eval-after-load 'lsp-ui
  (define-key lsp-mode-map (kbd "C-h D") 'lsp-describe-thing-at-point)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))



(provide 'init-lsp)
