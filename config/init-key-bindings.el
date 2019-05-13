;;; init-key-bindings.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; (cond ((featurep 'cocoa)
;;        ;; 交换 option 和 command 键
;;        (setq mac-option-modifier 'super)
;;        (setq mac-command-modifier 'meta)
;;        (setq mac-right-command-modifier 'ctrl)
;;        )

;;       ((eq system-type 'windows-nt)
;;        ;; Make PC keyboards Win key or other to type Super or Hyper
;;        (setq w32-pass-lwindow-to-system nil)
;;        (setq w32-lwindow-modifire 'super)
;;        (setq w32-apps-modifier 'hyper)

;;        (w32-register-hot-key [s-])
;;        (w32-register-hot-key [s-t])
;;        )
;;       )

; 全局按键卸载
;;(lazy-load-unset-keys
;; '("C-x C-f" "C-z" "C-q" "s-W" "s-z" "M-h" "C-x C-c" "C-\\" "s-c" "s-x" "s-v"))

;; Dash
(lazy-load-global-keys
 '(
   ("D" . dash-at-point)
   )
 "dash-at-point"
 "C-c"
 )

;; symbol-overlay
(lazy-load-set-keys
 '(
   ("M-i" . symbol-overlay-put)
   ("M-n" . symbol-overlay-jump-next)
   ("M-p" . symbol-overlay-jump-prev)
   ("M-s r" . symbol-overlay-rename)
   ("C-g" . symbole-overlay-remove-all)
   )
 symbol-overlay-mode-map
 )

(lazy-load-unset-keys
 '([M-left] [M-right]))





(provide 'init-key-bindings)
;;; init-key-bindings.el ends here
