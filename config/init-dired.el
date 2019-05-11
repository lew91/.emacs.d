(require 'dired)

(setq dired-recursive-copies t)    ;可以递归的进行拷贝
(setq dired-recursive-deletes t)   ;可以递归的删除目录
(setq dired-recursive-deletes 'always)  ;删除东西时不提示
(setq dired-recursive-copies 'always) ;拷贝东西时不提示

(setq-default dired-dwim-target t)

(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(after-load 'dired
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (define-key dired-mode-map (kbd "C-c C-q") 'wdired-change-to-wdired-mode))

(require 'diff-hl)
(after-load 'dired
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

(provide 'init-dired)
;; init-dired.el ends here
