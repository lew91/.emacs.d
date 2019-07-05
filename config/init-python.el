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

(with-eval-after-load 'python
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(with-eval-after-load 'anaconda-mode
    (define-key anaconda-mode-map (kbd "M-?") nil))


(with-eval-after-load 'company
      (with-eval-after-load 'python
        (push 'company-anaconda company-backends)))


(provide 'init-python)
;;; init-python.el ends here
