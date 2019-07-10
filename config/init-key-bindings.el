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

(require 'which-key)
(which-key-mode)
(diminish 'which-key-mode)

;; Misc 
;;(require 'bind-key)

;; handy key bindings
(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)

(global-set-key (kbd "C-;") 'avy-goto-char-timer)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Train myself to use M-f and M-b instead
(global-unset-key [M-left])
(global-unset-key [M-right])


;; Zap *up* to char is a handy pair for zap-to-char
(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-Z") 'zap-up-to-char)


(global-set-key (kbd "M-Y") 'browse-kill-ring)
(with-eval-after-load 'browse-kill-ring
  (define-key browse-kill-ring-mode-map (kbd "C-g") 'browse-kill-ring-quit)
  (define-key browse-kill-ring-mode-map (kbd "M-n") 'browse-kill-ring-forward)
  (define-key browse-kill-ring-mode-map (kbd "M-p") 'browse-kill-ring-previous))

;; Shift lines up and down with M-up and M-down. When paredit is enabled,
;; it will use those keybindings. For this reason, you might prefer to
;; use M-S-up and M-S-down, which will work even in lisp modes.
(global-set-key [M-up] 'md-move-lines-up)
(global-set-key [M-down] 'md-move-lines-down)
(global-set-key [M-S-up] 'md-move-lines-up)
(global-set-key [M-S-down] 'md-move-lines-down)

(global-set-key (kbd "C-c d") 'md-duplicate-down)
(global-set-key (kbd "C-c u") 'md-duplicate-up)


;; multiple-cursors
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-+") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; From active region to multiple cursors:
(global-set-key (kbd "C-c m r") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "C-c m e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c m a") 'mc/edit-beginnings-of-lines)


(with-eval-after-load 'symbol-overlay
  (define-key symbol-overlay-mode-map (kbd "M-i") 'symbol-overlay-put)
  (define-key symbol-overlay-mode-map (kbd "M-n") 'symbol-overlay-jump-next)
  (define-key symbol-overlay-mode-map (kbd "M-p") 'symbol-overlay-jump-prev)
  (define-key symbol-overlay-mode-map (kbd "M-s r") 'symbol-overlay-rename)
  (define-key symbol-overlay-mode-map (kbd "C-g") 'symbol-overlay-remove-all))

;; org
(with-eval-after-load 'org
  (define-key global-map (kbd "C-c l") 'org-store-link)
  (define-key global-map (kbd "C-c a") 'org-agenda))
(global-set-key (kbd "C-c c") 'org-capture)





(provide 'init-key-bindings)
;;; init-key-bindings.el ends here
