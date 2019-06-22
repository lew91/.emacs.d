(require 'cc-mode)
(require 'semantic)
(require-package 'company-c-headers)
(require-package 'ggtags)

(setq c-default-style "k&r")

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-stickyfunc-mode 1)
(semantic-mode 1)
(ggtags-mode 1)


(defun lew/cedet-hook ()
  (local-set-key "\C-c\C-j" 'semantic-ia-fast-jump)
  (local-set-key "\C-c\C-s" 'semantic-ia-show-summary))

(add-hook 'c-mode-common-hook 'lew/cedet-hook)
(add-hook 'c-mode-hook 'lew/cedet-hook)
(add-hook 'c++-mode-hook 'lew/cedet-hook)

;; Enable EDE only in C/C++
(require 'ede)
(global-ede-mode)

(add-to-list 'company-backends 'company-c-headers)
(add-hook 'c-mode-common-hook 'hs-minor-mode)

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))

;; Find definitions in current buffer
(setq-local imenu-create-index-function #'ggtags-build-imenu-index)

(setq speedbar-show-unknown-files t)

(dolist (map (list ggtags-mode-map dired-mode-map))
  (define-key map (kbd "C-c g s") 'ggtags-find-other-symbol)
  (define-key map (kbd "C-c g h") 'ggtags-view-tag-history)
  (define-key map (kbd "C-c g r") 'ggtags-find-reference)
  (define-key map (kbd "C-c g f") 'ggtags-find-file)
  (define-key map (kbd "C-c g c") 'ggtags-create-tags)
  (define-key map (kbd "C-c g u") 'ggtags-update-tags)
  (define-key map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
  (define-key map (kbd "M-.") 'ggtags-find-tag-dwim)
  (define-key map (kbd "M-,") 'pop-tag-mark)
  (define-key map (kbd "C-c <") 'ggtags-prev-mark)
  (define-key map (kbd "C-c >") 'ggtags-next-mark))

;; setup GDB
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )


(provide 'init-cedet)
