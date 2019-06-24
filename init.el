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
    (require 'setup-package)            ; 设定插件源和安装工具
    ;;(require 'selected-packages)      ; 只需初始安装时加载一次,extensions文件夹使用 git submodule update --init --recursive 更新使用
    (require 'init-exec-path)
    (require 'init-auto-save)
    (require 'basic-edit-toolkit)

    (require 'init-fonts)              ;字体集，中英文对齐
    (require 'init-grep)
    (require 'init-smex)
    (require 'init-editing)
    (require 'init-hippie-expand)
    (require 'init-visual-regexp)
    (require 'init-dired)
    (require 'grep-dired)
    (require 'init-isearch)
    (require 'init-uniquify)
    (require 'init-ibuffer)
    (require 'init-window)
    (require 'init-smooth-scrolling)
    ;;(require 'aweshell)
    (require 'init-theme)
    (require 'init-session)
    (require 'init-key-bindings)


    ;; 可以延后加载的
    (run-with-idle-timer
     1 nil
     #'(lambda ()

         (require 'init-ivy)
         ;;(require 'init-helm)
         (require 'init-undo-tree)
         (require 'init-insert-translated-name)      ; 用‘insert-translated-name’激活
         (require 'company-english-helper)           ; 用‘toggle-company-english-helper’激活
         (require 'init-org)
         (require 'org-toolkits)                     ; 自定义一些很有用的函数
         (require 'init-markdown)
         (require 'init-tex)
         (require 'init-yasnippet)
         (require 'init-company)
         (require 'init-flycheck)
         (require 'init-ispell)
         (require 'init-origami)
         (require 'init-lsp)
         (require 'init-elisp)
         (require 'init-slime)
         (require 'init-common-lisp)
         (require 'init-paredit)
         ;;(require 'init-ycmd)                        ; just for C/C++
         (require 'init-python)
         (require 'init-eldoc)
         (require 'init-dash)
         (require 'init-sql)
         (require 'init-projectile)
         (require 'init-git)
         (require 'init-vc)
         (require 'init-whitespace)
         (require 'init-session)
         (emacs-session-restore)

         )))


  (setq custom-file (expand-file-name "custom.el" jakelew-emacs-root-dir))
  (load custom-file 'no-error 'no-message)

  )
