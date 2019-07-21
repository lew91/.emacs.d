(require 'yasnippet)
(require 'yasnippet-snippets)

;;; Code:


;; (defun get-git-user-name ()
;;   (interactive)
;;   (replace-regexp-in-string "\n$" "" (shell-command-to-string "git config --get user.name")))

;; (defun get-git-user-email ()
;;   (interactive)
;;   (replace-regexp-in-string "\n$" "" (shell-command-to-string "git config --get user.email")))

;;(setq yas-snippet-dirs (append yas-snippet-dirs
;;     (concat jl-emacs-root-dir "/snippets")  ; custom snippets
;;     ))

;;(yas-global-mode 1)

(eval-after-load "yasnippet"
  '(diminish 'yas-minor-mode " Y"))

(dolist (hook (list
               'prog-mode-hook
               'org-mode-hook))
  (add-hook hook '(lambda()
                    (yas-minor-mode 1))))

;; Disable yasnippet mode on some mode.
(dolist (hook (list
               'term-mode-hook
               'shell-mode-hook
               ))
  (add-hook hook '(lambda () (yas-minor-mode -1))))

(provide 'init-yasnippet)
