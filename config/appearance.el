;; 去掉工具栏，滚动栏。Mac OS 保留菜单栏
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (and (not (eq system-type 'darwin)) (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Restore emacs session.
(setq initial-buffer-choice t)
(run-with-timer 1 nil #'(lambda () (bury-buffer)))

(fset 'yes-or-no-p 'y-or-n-p)           ;以 y/n代表 yes/no
(blink-cursor-mode -1)                  ;指针不闪动
(transient-mark-mode 1)                 ;标记高亮
(global-hl-line-mode 1)                 ;高亮当前行
(setq use-dialog-box nil)               ;never pop dialog
(setq use-file-dialog nil)              ;文件操作不需要
(setq inhibit-startup-screen t)         ;inhibit start screen
(setq initial-scratch-message "")       ;关闭启动空白buffer, 这个buffer会干扰session恢复
(setq-default comment-style 'indent)    ;设定自动缩进的注释风格
(setq ring-bell-function 'ignore)       ;关闭烦人的出错时的提示声
(setq default-major-mode 'text-mode)    ;设置默认地主模式为TEXT模式
(setq mouse-yank-at-point t)            ;粘贴于光标处,而不是鼠标指针处
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))  ;一次滚动一行
(setq mouse-wheel-progressive-speed nil)
(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000)
(setq x-select-enable-clipboard t)      ;支持emacs和外部程序的粘贴
(setq split-width-threshold nil)        ;分屏的时候使用上下分屏
(setq inhibit-compacting-font-caches t) ;使用字体缓存，避免卡顿
(setq profiler-report-cpu-line-format ;让 profiler-report 第一列宽一点
      '((100 left)
        (24 right ((19 right)
                   (5 right)))))
(setq profiler-report-memory-line-format
      '((100 left)
        (19 right ((14 right profiler-format-number)
                   (5 right)))))

(when (boundp 'ns-pop-up-frames)      ; Don't open a file in a new frame
  (setq ns-pop-up-frames nil))
;;设置标题拦格式
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
;; mac os 使用原生全屏
(when (eq system-type 'darwin)
  (setq ns-use-native-fullscreen nil))
;; 标题栏透明
(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))




;; 不显示 *scratch*
(defun remove-scratch-buffer ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))




;;; Whitespace
(setq-default show-trailing-whitespace nil)
(defun sanityinc/show-trailing-whitespace ()
  "Enable display of trailing whitespace in this buffer."
  (setq-local show-trailing-whitespace t))

(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook 'sanityinc/show-trailing-whitespace))

;; Misc

;;(autoload 'zap-to-char "misc"
;;  "Kill up to, but not including ARGth occurrence of CHAR." t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

;; tracking recent files
(add-hook 'after-init-hook 'recentf-mode)
(setq-default
 recentf-max-saved-items 1000
 recentf-exclude '("/tmp/" "/ssh:"))
;;(add-to-list 'recentf-exclude "\\.png\\'")

(let ((list-partern (list
                    '("\\.png\\'")
                    '("\\.revive\\'")
                    ))
      )
  (add-to-list  'recentf-exclude 'list-partern))


;; 括号匹配开启
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
    (ido-everywhere nil)
    (setq ido-enable-flex-matching t)
    (setq ido-create-new-buffer 'always)
    )
  )





(provide 'appearance)
