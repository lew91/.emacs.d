
;; 设置加载路径
(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))

;;elpa 目录
(add-subdirs-to-load-path "~/.emacs.d/elpa/")
;; 额外插件路径
(add-subdirs-to-load-path "~/.emacs.d/extensions/")
;; 配置文件主目录
(add-to-list 'load-path "~/.emacs.d/config")

;; 统一使用after-load 
(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

(defconst *is-a-mac* (eq system-type 'darwin))



(let (
      ;; 加载的时候临时增大`gc-cons-threshold'以加速启动速度。
      (gc-cons-threshold most-positive-fixnum)
      ;; 清空避免加载远程文件的时候分析文件。
      (file-name-handler-alist nil))
  ;; 定义一些启动目录，方便下次迁移修改
  (defvar jakelew-emacs-root-dir (file-truename "~/.emacs.d"))
  (defvar jakelew-emacs-config-dir (concat jakelew-emacs-root-dir "/config"))
  (defvar jakelew-emacs-extension-dir (concat jakelew-emacs-root-dir "/extensions"))
  (defvar jakelew-emacs-sdcv-data-dir (concat jakelew-emacs-root-dir "/sdcv-dict"))

  (with-temp-message ""                 ;抹掉插件启动的输出
    (require 'init-benchmarking)

    ;; 先设置背景，避免闪烁。
    ;;(custom-set-faces
    ;; '(default ((t (:background "black" :foreground "#137D11"))))
   ;; )

    (require 'appearance)                ; 加载初始化基本外观
    (require 'cache-path-from-shell)     ; 使 exec-paht-from-shell 只加载一次
    (require 'lazy-load)                 ; 延迟加载，加快启动速度
    (require 'setup-package)            ; 设定插件源和安装工具
    ;;(require 'selected-packages)      ; 只需初始安装时加载一次
    (require 'aweshell)                 ; 增强eshell, 自动补全等
    (require 'init-auto-save)

    ;;(require 'one-key)
    ;; (require 'awesome-pair)
    ;; (require 'basic-toolkit)
    ;; (require 'redo)

    (require 'init-fonts)              ; 设置字体集
    ;;(require 'init-awesome-tray)
    ;; (require 'init-awesome-tab)
    ;; (require 'init-backup)
    (require 'init-grep)
    (require 'init-smex)
    (require 'init-editing-utils)
    ;; (require 'init-line-number)
    
    ;; (require 'init-mode)
    (require 'init-dired)
    (require 'init-isearch)
    (require 'init-uniquify)
    (require 'init-ibuffer)
    (require 'init-window)
    (require 'init-session)
    ;; (require 'init-awesome-pair)
    ;; (require 'init-indent)
    ;; (require 'init-one-key)
    ;; (require 'init-iedit)
    ;; (require 'init-visual-regexp)
    ;; (require 'init-key)
    ;; (require 'init-vi-navigate)
    ;; (require 'init-performance)
    ;; (require 'init-pyim)
    ;; (require 'init-sdcv)
    ;; (require 'init-insert-translated-name)

    ;; 可以延后加载的
    (run-with-idle-timer
     1 nil
     #'(lambda ()
         (require 'jakelew-org)
         ;; (require 'pretty-lambdada)
         ;; (require 'browse-kill-ring)
         ;; (require 'elf-mode)

         ;; (require 'init-tempbuf)
         ;; (require 'init-eldoc)
         ;; (require 'init-doxymacs)
         (require 'init-yasnippet)
         (require 'init-company-mode)
         ;; ;; (require 'init-lsp)
         ;; (require 'init-package)
         ;; (require 'init-smooth-scrolling)
         ;; (require 'init-cursor-chg)
         ;; (require 'init-winpoint)
         ;; (require 'init-benchmark)
         ;; (require 'init-info)
         ;; (require 'init-auto-sudoedit)
         ;; (require 'init-atomic-chrome)
         ;; (require 'init-qt)
         (require 'init-flycheck)

         (require 'init-python)

         ;; (require 'init-idle)
         (require 'init-session)
         ;; Restore session at last.
         (emacs-session-restore)
         ))
    ))


;; Emacs 自动生成以及自定义内容
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (spacemacs-dark)))
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(package-selected-packages
   (quote
    (flycheck-popup-tip hl-todo flycheck-pos-tip yasnippet-snippets ag rg wgrep-ag company-posframe paradox highlight-parentheses yaml-mode writeroom-mode whole-line-or-region whitespace-cleanup-mode wgrep vlf vc-darcs uptimes unfill textile-mode symbol-overlay switch-window sqlformat spacemacs-theme smex session scratch regex-tool rainbow-mode rainbow-delimiters pip-requirements paredit-everywhere page-break-lines osx-location origami org-pomodoro org-cliplink org-bullets ns-auto-titlebar multiple-cursors move-dup mode-line-bell mmm-mode markdown-mode macrostep list-unicode-display ledger-mode ipretty info-colors immortal-scratch ibuffer-vc ibuffer-projectile hydra highlight-quoted highlight-escape-sequences helm-swoop helm-projectile helm-flx helm-descbinds helm-ag guide-key grab-mac-link goto-line-preview gnuplot fullframe flycheck-package flycheck-ledger flycheck-color-mode-line expand-region exec-path-from-shell elisp-slime-nav dotenv-mode disable-mouse diredfl dimmer diminish diff-hl default-text-scale dash-at-point darcsum daemons company-quickhelp company-anaconda command-log-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized cmd-to-echo cl-libify cl-lib-highlight cask-mode browse-kill-ring browse-at-remote bind-key beacon avy auto-compile anzu aggressive-indent)))
 '(python-shell-buffer-name "Python3")
 '(python-shell-completion-native-disabled-interpreters (quote ("pypy" "ipython" "python3" "python")))
 '(python-shell-completion-native-enable t)
 '(python-shell-interpreter "python3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
