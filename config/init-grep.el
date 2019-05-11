;;; init-grep.el --- Settings for grep and grep-like tools -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:



(setq-default grep-highlight-matches t
              grep-scroll-output t)

(when *is-a-mac*
  (setq-default locate-command "mdfind"))

(require 'wgrep)
(after-load 'grep
  (dolist (key (list (kbd "C-c C-q") (kbd "w")))
    (define-key grep-mode-map key 'wgrep-change-to-wgrep-mode)))

(when (and (executable-find "ag")
           (require 'ag))
  (require 'wgrep-ag)
  (setq-default ag-highlight-search t)
  (global-set-key (kbd "M-?") 'ag-project))

(when (and (executable-find "rg")
           (require 'rg))
  (require 'deadgrep)
  (global-set-key (kbd "M-?") 'rg-project))


(provide 'init-grep)
;;; init-grep.el ends here

