(require 'ivy)
(require 'ivy-xref)
(require 'swiper)
(require 'counsel)
(require 'projectile)


(add-hook 'after-init-hook 'ivy-mode)
(add-hook 'after-init-hook 'counsel-mode)


(with-eval-after-load 'ivy
  (diminish 'ivy-mode)

  (setq-default ivy-use-virtual-buffers t
                ivy-virtual-abbreviate 'fullpath
                ivy-count-format ""
                projectile-completion-system 'ivy
                ivy-magic-tilde nil
                ivy-dynamic-exhibit-delay-ms 150
                ivy-use-selectable-prompt t
                ivy-initial-inputs-alist
                '((Man-completion-table . "^")
                  (woman . "^")))

  (setq xref-show-xrefs-function 'ivy-xref-show-xrefs)

  ;; IDO-style directory navigation
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (dolist (k '("C-j" "C-RET"))
    (define-key ivy-minibuffer-map (kbd k) #'ivy-immediate-done))

  (define-key ivy-minibuffer-map (kbd "<up>") #'ivy-previous-line-or-history)

  (define-key ivy-occur-mode-map (kbd "C-c C-q") #'ivy-wgrep-change-to-wgrep-mode)
  )


(with-eval-after-load 'swiper
  (define-key ivy-mode-map (kbd "M-s /") 'swiper-thing-at-point))


(with-eval-after-load 'counsel
  (setq-default counsel-mode-override-describe-bindings t)
  (diminish 'counsel-mode)

  (with-eval-after-load 'projectile
    (let ((search-function
           (cond
            ((executable-find "rg") 'counsel-rg)
            ((executable-find "ag") 'counsel-ag)
            ((executable-find "pt") 'counsel-pt)
            ((executable-find "ack") 'counsel-ack))))
      (when search-function
        (defun jl/counsel-search-project (initial-input &optional use-current-dir)
          "Search using `counsel-rg' or similar from the project root for INITIAL-INPUT.
If there is no project root, or if the prefix argument
USE-CURRENT-DIR is set, then search from the current directory
instead."
          (interactive (list (thing-at-point 'symbol)
                             current-prefix-arg))
          (let ((current-prefix-arg)
                (dir (if use-current-dir
                         default-directory
                       (condition-case err
                           (projectile-project-root)
                         (error default-directory)))))
            (funcall search-function initial-input dir)))))
    (with-eval-after-load 'ivy
      (add-to-list 'ivy-height-alist (cons 'counsel-ag 20)))

    (global-set-key (kbd "M-?") 'jl/counsel-search-project)))


(provide 'init-ivy)
