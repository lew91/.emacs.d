;;; init-key-bindings.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(cond ((featurep 'cocoa)
       ;; 交换 option 和 command 键
       (setq mac-option-modifier 'super)
       (setq mac-command-modifier 'meta)
       (setq mac-right-command-modifier 'ctrl)
       )

      ((eq system-type 'windows-nt)
       ;; Make PC keyboards Win key or other to type Super or Hyper
       (setq w32-pass-lwindow-to-system nil)
       (setq w32-lwindow-modifire 'super)
       (setq w32-apps-modifier 'hyper)

       (w32-register-hot-key [s-])
       (w32-register-hot-key [s-t])
       )
      )




(provide 'init-key-bindings)
;;; init-key-bindings.el ends here
