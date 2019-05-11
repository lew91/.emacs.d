(require 'ipsell)
;;(require 'flyspell)

(when (executable-find ispell-program-name)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(with-eval-after-load 'flyspell
  (setq flyspell-issue-message-flag nil)
  (setq-default ispell-program-name "aspell")
  (setq-default ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")))

(provide 'init-ispell)
