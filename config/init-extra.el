;;; init-extra.el --- extra extensions that could be required -*- lexical-binding: t -*-
;;
;;; Commentary:
;;; Additionally extensions that use git submodule added, or user's custom plugins.
;;; Such as user custom configuration codes can also be placed here


;;; Code:

(require 'grep-dired)              ; Usage: M-x 'grep-dired', find file in dired-mode
(require 'company-english-helper)  ; Usage: M-x 'toggle-company-english-helper'
(require 'insert-translated-name)  ; Usage: M-x  'insert-translated-name'

(dolist (hook (list
               'org-mode-hook
               'markdown-mode-hook
               'prog-mode-hook
               'text-mode-hook
               ))
  (add-hook hook '(lambda ()
                    (insert-translated-name-use-original-translation))
            ))

(provide 'init-extra)
;;; init-extra.el ends here
