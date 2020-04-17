;;; init-lsp.el --- Language server protocol configuration  -*- lexical-binding: t -*-

;;; Commentary: List of LSP Supported languages
;; those can see in  https://github.com/emacs-lsp/lsp-mode or
;; https://github.com/joaotavora/eglot

;; NOTE: For satisfy Python develop environment under LSP

;; should install those packages
;; pure python impelement of language server
;; pip3 install python-language-server

;; pip3 install pyflakes pylint mccabe pydocstyle yapf rope

;;; Code:

(require 'eglot)
(require 'lsp-mode)
(require 'lsp-clients)
(require 'lsp-ui)
(require 'company-lsp)
;;(require 'lsp-python-ms)


(setq lsp-auto-guess-root nil             ; default is nil, wanna auto indicator? set it t
      lsp-prefer-flymake t              ; Use flycheck set it nil, otherwise t for flymake
      flymake-fringe-indicator-position 'left-fringe)


(setq lsp-ui-doc-header t
      lsp-ui-doc-include-signature t
      lsp-ui-doc-enable t
      lsp-ui-doc-delay 1.0
      lsp-ui-doc-postition 'at-point)

;; TODO: disable client-side cache, the LSP server does a better job
(with-eval-after-load 'lsp-mode
  (with-eval-after-load 'company
    (push 'company-lsp company-backends)
    (setq company-lsp-async t)
    (setq company-lsp-cache-candidates 'auto)))


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



;;; Languages

;; Python3, Microsoft language Server
;; Manual build executable server references from https://github.com/emacs-lsp/lsp-python-ms
;;(setq lsp-python-executable-cmd "python3")
;;(setq lsp-python-ms-executable "~/python-language-server/output/bin/Release/osx-x64/publish/Microsoft.Python.LanguageServer")

;; clangd
(when (equal system-type 'darwin)
  ;;(setq lsp-clients-clangd-executable "/usr/local/opt/llvm/bin/clangd"))
  (setq lsp-clients-clangd-executable "clangd"))



;; eglot 
(add-hook 'python-mode-hook 'eglot-ensure)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)



(provide 'init-lsp)
;;; init-lsp.el ends here
