(require 'projectile)

(add-hook 'after-init-hook 'projectile-mode)
(setq-default projectile-mode-line-prefix " Proj")

(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(provide 'init-projectile)
