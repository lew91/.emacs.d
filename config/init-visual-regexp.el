
(require 'visual-regexp)

(setq vr/match-separator-use-custom-face t)
(setq vr/match-separator-string "⇛")

(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)

(define-key global-map (kbd "C-c m m") 'vr/mc-mark) ; 配合multiple-cursors 使用


(provide 'init-visual-regexp)
