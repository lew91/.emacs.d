(require 'ispell)
;;(require 'flyspell)

(when (executable-find ispell-program-name)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(with-eval-after-load 'flyspell
  (setq flyspell-issue-message-flag nil)
  (define-key flyspell-mode-map (kbd "C-;") nil)  ; 按键与avy-jump 冲突，取消之
  (define-key flyspell-mode-map (kbd "C-.") nil)  ; 与全局set-mark 冲突，取消之
  (setq-default ispell-program-name "aspell")
  (setq-default ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")))

 

(provide 'init-ispell)
