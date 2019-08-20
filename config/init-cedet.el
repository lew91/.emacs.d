(require 'cc-mode)
(require 'semantic)
(require 'company-c-headers)


(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-stickyfunc-mode 1)


(semantic-mode 1)


(defun jl/cedet-hook ()
  (local-set-key "\C-c\C-j" 'semantic-ia-fast-jump)
  (local-set-key "\C-c\C-s" 'semantic-ia-show-summary))

(add-hook 'c-mode-common-hook 'jl/cedet-hook)
(add-hook 'c-mode-hook 'jl/cedet-hook)
(add-hook 'c++-mode-hook 'jl/cedet-hook)


(add-to-list 'company-backends 'company-c-headers)


(provide 'init-cedet)
