;;; init-python.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'pip-requirements)
(require 'anaconda-mode)
(require 'company-anaconda)

(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

(after-load 'python
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(after-load 'anaconda-mode
    (define-key anaconda-mode-map (kbd "M-?") nil))


(after-load 'company
      (after-load 'python
        (push 'company-anaconda company-backends)))


(provide 'init-python)
;;; init-python.el ends here
