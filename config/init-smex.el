;;; init-smex.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'smex)

;;; Code:

(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
;; (lazy-load-set-keys
;;  '(
;;    ("M-x" . smex)
;;    ("M-X" . smex-major-mode-commands)
;;    ("C-c C-c M-x" . execute-extended-command)
;;    )
;;  )


(provide 'init-smex)

;;; init-smex.el ends here
