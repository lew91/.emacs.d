(defun add-subdirs-to-load-path (dir)
  "Recursive add directories DIR to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))


(let (
      ;; 加载的时候临时增大`gc-cons-threshold'以加速启动速度。
      (gc-cons-threshold most-positive-fixnum)
      ;; 清空避免加载远程文件的时候分析文件。
      (file-name-handler-alist nil))
  ;; 定义一些启动目录，方便下次迁移修改
  (defvar jl-emacs-root-dir (file-truename "~/.emacs.d"))
  (defvar jl-emacs-config-dir (concat jl-emacs-root-dir "/config"))
  (defvar jl-emacs-extension-dir (concat jl-emacs-root-dir "/extensions"))

  (add-subdirs-to-load-path (expand-file-name "elpa" jl-emacs-root-dir))
  (add-subdirs-to-load-path jl-emacs-extension-dir)
  (add-to-list 'load-path jl-emacs-config-dir)



  (require 'benchmark-init)               ;启动时间测试
  (add-hook 'after-init-hook 'benchmark-init/deactivate)  ; 启动后停止激活状态

  (with-temp-message ""                 ;抹掉插件启动的输出

    (require 'appearance)
    (require 'basic-utils)               ; 基本工具集，在加载其他模块前加载
    (require 'setup-package)            ; 设定插件源和安装工具
    ;;(require 'selected-packages)      ; 只需初始安装时加载一次,extensions文件夹使用 git submodule update --init --recursive 更新使用

    (require 'init-exec-path)
    ;;(require 'init-auto-save)
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
    (require 'init-theme)
    (require 'init-session)
    (require 'init-key-bindings)


    ;; 可以延后加载的
    (run-with-idle-timer
     1 nil
     #'(lambda ()

         (require 'init-mmm)
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
         ;;(require 'init-python)
         (require 'init-eldoc)
         (require 'init-dash)
         (require 'init-projectile)
         (require 'init-git)
         (require 'init-vc)
         (require 'init-whitespace)
         (require 'init-session)
         (emacs-session-restore)

         )))


  (setq custom-file (expand-file-name "custom.el" jl-emacs-root-dir))
  (load custom-file 'no-error 'no-message)

  )
