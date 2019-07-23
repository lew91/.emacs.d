(defun add-subdirs-to-load-path (dir)
  "Recursive add directories DIR to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))


(let (
      (gc-cons-threshold most-positive-fixnum)
      (file-name-handler-alist nil))
  ;; Comparative more migratable
  (defvar jl-emacs-root-dir (file-truename "~/.emacs.d"))
  (defvar jl-emacs-config-dir (concat jl-emacs-root-dir "/config"))
  (defvar jl-emacs-extension-dir (concat jl-emacs-root-dir "/extensions"))

  (add-subdirs-to-load-path (expand-file-name "elpa" user-emacs-directory))
  (add-subdirs-to-load-path jl-emacs-extension-dir)
  (add-to-list 'load-path jl-emacs-config-dir)



  (require 'benchmark-init)               ; Test the Emacs startup time
  (add-hook 'after-init-hook 'benchmark-init/deactivate)  ; Deactivate after it loaded

  (with-temp-message ""                 ; No messages when those modules started

    (require 'init-appearance)
    (require 'init-package)            ; package-archives
    (require 'init-shell)
    ;;(require 'init-fonts)              ; Set of fonts, especially Chinese and English fonts aligned
    (require 'init-grep)
    (require 'init-smex)
    (require 'init-editing)
    (require 'init-hippie-expand)
    (require 'init-visual-regexp)
    (require 'init-dired)
    (require 'init-isearch)
    (require 'init-ibuffer)
    (require 'init-window)
    (require 'init-theme)
    (require 'init-key-bindings)


    ;; Those could be defer loaded
    (run-with-idle-timer
     1 nil
     #'(lambda ()

         (require 'init-mmm)
         (require 'init-ivy)
         ;;(require 'init-helm)
         (require 'init-undo-tree)
         (require 'init-org)
         (require 'init-org-toolkits)                     ; Customize useful functions
         (require 'init-markdown)
         (require 'init-tex)
         (require 'init-yasnippet)
         (require 'init-company)
         (require 'init-ispell)
         (require 'init-lsp)
         (require 'init-elisp)
         (require 'init-slime)
         (require 'init-common-lisp)
         (require 'init-paredit)
         ;;(require 'init-python)
         (require 'init-eldoc)
         (require 'init-dash)
         (require 'init-projectile)
         (require 'init-git)
         (require 'init-whitespace)
         (require 'init-extra nil t)                ; Extra extensions that use git submodule added, or user's additionally customize

         )))


  (setq custom-file (expand-file-name "custom.el" jl-emacs-root-dir))
  (load custom-file 'no-error 'no-message)

  )
