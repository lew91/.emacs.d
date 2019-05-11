(require 'auto-save)

(auto-save-enable)
(setq auto-save-silent t)
(setq auto-save-delete-trailing-whitespace t)

;; 不要自动备份
(setq make-backup-files nil)
(setq auto-save-default nil)

(provide 'init-auto-save)
