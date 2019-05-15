(require 'flycheck)

(require 'flycheck-pos-tip)
(add-hook 'flycheck-mode-hook 'flycheck-pos-tip-mode)
(setq flycheck-pos-tip-timeout 30)

(require 'flycheck-popup-tip)
(add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OS Config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (featurep 'cocoa)
  ;; Initialize environment from user's shell to make eshell know every PATH by other shell.
  (require 'exec-path-from-shell)
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "GEM_PATH"))
  (exec-path-from-shell-initialize))

;; I don't like `global-flycheck-mode', some mode, such as elisp mode don't need.
(dolist (hook (list
               'ruby-mode-hook
               'python-mode-hook
               ))
  (add-hook hook '(lambda () (flycheck-mode 1))))

;;(setq flycheck-indication-mode 'right-fringe)
(setq flycheck-indication-mode 'left-fringe)
(setq flycheck-emacs-lisp-load-path 'inherit)
(setq filcheck-check-syantax-automatically '(save mode-enabled)) ;只在打开和保存时调用

;; hydra for flycheck
;; https://github.com/abo-abo/hydra/wiki/Flycheck
;; (defhydra hydra-flycheck
;;     (:pre (progn (setq hydra-lv t) (flycheck-list-errors))
;;           :post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
;;           :hint nil)
;;     "Errors"
;;     ("f"  flycheck-error-list-set-filter                            "Filter")
;;     ("j"  flycheck-next-error                                       "Next")
;;     ("k"  flycheck-previous-error                                   "Previous")
;;     ("gg" flycheck-first-error                                      "First")
;;     ("G"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
;;     ("w"  flycheck-copy-erros-as-kill                        "copy message")
;;     ;; See 'helm-flycheck package below
;;     ("h"  helm-flycheck                                       "list with helm")
;;     ("q"  nil))

;;   (define-key flycheck-mode-map (kbd "C-c e") 'hydra-flycheck/body)

;; 外观更漂亮一点
(with-eval-after-load 'flycheck
  (progn
    (define-fringe-bitmap 'my-flycheck-fringe-indicator
      (vector #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00011100
              #b00111110
              #b00111110
              #b00111110
              #b00011100
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000))

    (flycheck-define-error-level 'error
      :severity 2
      :overlay-category 'flycheck-error-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-error)

    (flycheck-define-error-level 'warning
      :severity 1
      :overlay-category 'flycheck-warning-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-warning)

    (flycheck-define-error-level 'info
      :severity 0
      :overlay-category 'flycheck-info-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-info)))






(provide 'init-flycheck)
;; init-flycheck.el ends here
