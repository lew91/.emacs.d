;; Get rid of tool bar, menu bar and scroll bars.
;; On Mac OS X we preserve the menu bar.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (and (not (eq system-type 'darwin)) (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Restore emacs session.
;;(setq initial-buffer-choice t)
;;(run-with-timer 1 nil #'(lambda () (bury-buffer)))

;; Opt out from the startup message in the echo area by simply
;; disabling this ridiculously bizarre thing entirely
(fset 'display-startup-echo-area-message #'ignore)


(fset 'yes-or-no-p 'y-or-n-p)
(blink-cursor-mode -1)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)         ;inhibit start screen
;;(setq initial-scratch-message (concat ";; Happly hacking! " user-login-name "- Emacs ♥ you!\n\n"))
(setq inhibit-compacting-font-caches t)

(setq initial-scratch-message (concat "
;;                _.-^^---....,,--
;;            _--                  --_
;;           <          SONIC         >)
;;           |       BOOOOOOOOM!       |
;;            \\._                   _./
;;               ```--. . , ; .--'''
;;                     | |   |
;;                  .-=||  | |=-.
;;                  `-=#$%&%$#=-'
;;                     | ;  :|
;;            _____.,-#%&$@%#&#~,._____
;;                Happly hacking !!!
;;              " user-login-name
                    " - Emacs ♥ you!\n\n"))

(setq use-dialog-box nil)               ;never pop dialog
(setq use-file-dialog nil)


(transient-mark-mode 1)
(global-hl-line-mode 1)
(setq-default comment-style 'indent)
;;(setq default-major-mode 'text-mode)


(setq mouse-yank-at-point t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000)
(setq select-enable-clipboard t)
(setq split-width-threshold nil)



(setq profiler-report-cpu-line-format
      '((100 left)
        (24 right ((19 right)
                   (5 right)))))
(setq profiler-report-memory-line-format
      '((100 left)
        (19 right ((14 right profiler-format-number)
                   (5 right)))))



(when (boundp 'ns-pop-up-frames)      ; Don't open a file in a new frame
  (setq ns-pop-up-frames nil))

;;Title format
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))


;; Title bar transparent on Mac
(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))


;; Use the original fullscreen on Mac OS X
(when (eq system-type 'darwin)
  (setq ns-use-native-fullscreen nil))


;; Nicer naming of buffers for files with identical names
(require 'uniquify)
;;(setq uniquify-buffer-name-style 'forward)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(require 'saveplace)
(setq-default save-place t)



;; tracking recent files
(add-hook 'after-init-hook 'recentf-mode)
(setq-default
 recentf-max-saved-items 300
 recentf-exclude '("/tmp/" "/ssh:"))
;;(add-to-list 'recentf-exclude "\\.png\\'")
(setq recentf-auto-cleanup 'never
      recentf-auto-cleanup-timer (run-with-idle-timer 600 t
                                                      'recentf-save-file))
(add-hook 'fine-file-hook (lambda () (unless recentf-mode
                                   (recentf-mode)
                                   (recentf-track-opened-file))))
(let ((list-pattern (list
                     '("\\.png\\'")
                     '("\\.revive\\'")
                     '("\\recentf")
                     '("\\ido\\.last")
                     ))
      )
  (add-to-list  'recentf-exclude 'list-pattern))



(show-paren-mode 1)
(setq-default indent-tabs-mode nil)


(require 'savehist)
(setq enable-recursive-minibuffers t ; Allow commands in minibuffers
      history-length 1000
      savehist-additional-variables '(mark-ring
                                      global-mark-ring
                                      search-ring
                                      regexp-search-ring
                                      extended-command-history)
      savehist-autosave-interval 60)
;;(savehist-mode t)
(add-hook 'after-init-hook 'savehist-mode)

;;; ido
(progn
  (unless (fboundp 'helm-mode)
    (ido-mode t)
    (setq ido-enable-flex-matching t)
    (setq ido-create-new-buffer 'always)
    )
  )

;; Auto save and store all backup files in one folder, the backup files are useful sometimes.
(when (not (fboundp 'auto-save))
  (setq backup-directory-alist `((".*" . ,(locate-user-emacs-file ".backup"))))
  (setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))))

;; Allow access from emacsclient
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))


(provide 'init-appearance)
