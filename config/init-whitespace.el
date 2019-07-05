
(require 'whitespace-cleanup-mode)

(setq-default show-trailing-whitespace nil)


;;; Whitespace

(defun jl/show-trailing-whitespace ()
  "Enable display of trailing whitespace in this buffer."
  (setq-local show-trailing-whitespace t))

(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook 'jl/show-trailing-whitespace))



(add-hook 'after-init-hook 'global-whitespace-cleanup-mode)
(with-eval-after-load 'whitespace-cleanup-mode
  (diminish 'whitespace-cleanup-mode))

(global-set-key [remap just-one-space] 'cycle-spacing)


(provide 'init-whitespace)
;;; init-whitespace.el ends here
