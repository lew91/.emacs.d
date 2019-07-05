(require 'yaml-mode)

;;(add-auto-mode 'yaml-mode "\\.yml\\.erb\\'")
(add-to-list 'auto-mode-alist '("\\.yml\\.erb\\'" . yaml-mode))
(add-hook 'yaml-mode-hook 'goto-address-prog-mode)

(provide 'init-yaml)
