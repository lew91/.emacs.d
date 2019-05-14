;; 设置加载子目录路径
(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))

;; melpa 目录
(add-subdirs-to-load-path "~/.emacs.d/elpa/")
;; 额外插件路径
(add-subdirs-to-load-path "~/.emacs.d/extensions/")
;; 配置文件主目录
(add-to-list 'load-path "~/.emacs.d/config")




(let (
      ;; 加载的时候临时增大`gc-cons-threshold'以加速启动速度。
      (gc-cons-threshold most-positive-fixnum)
      ;; 清空避免加载远程文件的时候分析文件。
      (file-name-handler-alist nil))
  ;; 定义一些启动目录，方便下次迁移修改
  (defvar jakelew-emacs-root-dir (file-truename "~/.emacs.d"))
  (defvar jakelew-emacs-config-dir (concat jakelew-emacs-root-dir "/config"))
  (defvar jakelew-emacs-extension-dir (concat jakelew-emacs-root-dir "/extensions"))



  (require 'benchmark-init)               ;启动时间测试
  (add-hook 'after-init-hook 'benchmark-init/deactivate)  ; 启动后停止激活状态

  (with-temp-message ""                 ;抹掉插件启动的输出
    ;;(require 'init-benchmarking)      ;显示启动时间到 messages


    (require 'appearance)                ; 加载初始化基本外观
    (require 'basic-utils)               ; 基本工具集，在加载其他模块前加载
    (require 'cache-path-from-shell)     ; 使‘exec-path-from-shell.el' 只加载一次
    ;;(require 'lazy-load)                 ; TODO：延迟加载键绑定,暂时速度可以，后面再研究
    (require 'setup-package)            ; 设定插件源和安装工具
    ;;(require 'selected-packages)      ; 只需初始安装时加载一次,extensions文件夹使用 git submodule update --init --recursive 更新使用
    (require 'init-auto-save)
    ;; (require 'awesome-pair)
    ;; (require 'basic-edit-toolkit)

    (require 'init-fonts)              ; 设置字体集
    (require 'init-grep)
    (require 'init-smex)
    (require 'init-editing-utils)
    (require 'init-hippie-expand)
    (require 'init-visual-regexp)
    (require 'init-dired)
    (require 'grep-dired)
    (require 'init-isearch)
    (require 'init-uniquify)
    (require 'init-ibuffer)
    (require 'init-window)
    (require 'init-session)
    ;;(require 'init-key-bindings)           ; 以后增加快捷键绑定管理


    ;; 可以延后加载的
    (run-with-idle-timer
     1 nil
     #'(lambda ()

         ;;(require 'init-ivy)
         ;;(require 'init-helm)
         ;;(require 'init-theme)                     ; 安装一次主题到 ‘custom-set-variables'
         ;; (require 'init-awesome-tray)              ; 不要‘mode-line’,在加载主题后执行加载
         (require 'aweshell)                         ; 增强‘eshell’, 自动补全等
         (require 'init-undo-tree)
         (require 'init-insert-translated-name)      ; 用‘insert-translated-name’激活
         (require 'company-english-helper)           ; 用‘toggle-company-english-helper’激活
         (require 'jakelew-org)
         (require 'org-toolkits)                     ; 自定义一些很有用的函数
         (require 'init-markdown)
         (require 'init-yasnippet)
         (require 'init-company-mode)
         ;; ;; (require 'init-lsp)                   ；lsp 补全模式，不打算用于全局
         (require 'init-flycheck)
         (require 'init-ispell)                      ; 拼写检查，字典默认
         (require 'init-origami)                   ; 代码折叠设置
         (require 'init-lisp)
         (require 'init-paredit)
         (require 'init-python)
         (require 'init-eldoc)
         (require 'init-dash)
         (require 'init-sql)
         (require 'init-projectile)
         ;;(require 'init-git)                       ; magit 工具设定
         ;;(require 'init-vc)                        ; 版本控制
         ;;(require 'init-whitespace)
         (require 'init-session)
         (emacs-session-restore)

         )))


  (setq custom-file (expand-file-name "custom.el" jakelew-emacs-root-dir))
  (load custom-file 'no-error 'no-message)

  )
