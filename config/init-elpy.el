(require 'elpy)
(require 'yapf)

(elpy-enable)
(setq elpy-rpc-python-command "python3")
(setq elpy-syntax-check-command "pyflakes")


(add-hook 'python-mode-hook 'yapf-mode)

(provide 'init-elpy)
