(after-load 'cc-mode
  (require 'irony)
  (dolist (hook (list
                 'c-mode-hook
                 'c++-mode-hook
                 ))
    (add-hook hook '(lambda ()
                      (irony-mode))))

  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

  (defun sarcasm-irony-cdb-not-found (command &rest args)
    (when (eq command 'get-compile-options)
      (message "Irony: compile options not found!")
      nil))

  (setq-default irony-cdb-compilation-databases #'(irony-cdb-clang-complete
                                                   irony-cdb-libclang
                                                   sarcasm-irony-cdb-not-found))

  ;; Windows performance tweaks
  (when (boundp 'w32-pipe-read-delay)
    (setq w32-pipe-read-delay 0))
  ;; set the buffer size to 64k on windows (from the original 4k)
  (when (boundp 'w32-pipe-buffer-size)
    (setq irony-server-w32-pipe-buffer-size (* 64 1024)))

  (require 'company-irony)
  (setq company-irony-ignore-case 'smart)
  (add-to-list 'company-backends 'company-irony)

  (defun company-c-headers-path-user-irony ()
    " Return the user include paths for the current buffer."
    (when irony-mode
      (irony--extract-user-search-paths irony--compile-options
                                        irony--working-directory)))

  (setq company-c-headers-path-user #'company-c-headers-path-user-irony)
  ;;(require 'company-c-headers)
  ;;(add-to-list 'company-backends #'company-c-headers)

  (require 'irony-eldoc)
  (add-hook 'irony-mode-hook 'irony-eldoc)
  )

(after-load 'flycheck
  (require 'flycheck-irony)
  (add-hook 'flycheck-mode-hook 'flycheck-irony-setup))

(provide 'init-cc)
