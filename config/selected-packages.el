(require 'setup-package)

;; install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   '(
     yasnippet-snippets
     yasnippet
     yaml-mode
     writeroom-mode
     whole-line-or-region
     whitespace-cleanup-mode
     wgrep-ag
     wgrep
     vlf
     visual-regexp
     vc-darcs
     uptimes
     unfill
     undo-tree
     textile-mode
     symbol-overlay
     switch-window
     sqlformat
     smex
     smooth-scrolling
     slime-company
     slime
     session
     scratch
     rg
     regex-tool
     rainbow-mode
     rainbow-delimiters
     pip-requirements
     paredit-everywhere
     page-break-lines
     paradox
     osx-trash
     osx-location
     origami
     org-pomodoro
     org-cliplink
     org-bullets
     ns-auto-titlebar
     multiple-cursors
     move-dup
     mode-line-bell
     mmm-mode
     markdown-mode
     magit
     macrostep
     list-unicode-display
     ledger-mode
     ivy-xref
     ivy
     ipretty
     info-colors
     immortal-scratch
     ibuffer-vc
     ibuffer-projectile
     hydra
     hungry-delete
     hl-todo
     highlight-quoted
     highlight-escape-sequences
     helm-swoop helm-projectile
     helm-descbinds
     helm-ag
     guide-key
     grab-mac-link
     goto-line-preview
     gnuplot fullframe
     format-all
     flycheck-pos-tip
     flycheck-package
     flycheck-ledger
     flycheck-color-mode-line
     expand-region
     exec-path-from-shell
     elisp-slime-nav
     diminish
     diff-hl
     default-text-scale
     dash-at-point darcsum
     daemons
     company-quickhelp
     company-anaconda
     command-log-mode
     cmd-to-echo
     counsel
     cl-libify
     cl-lib-highlight
     cal-china-x
     browse-kill-ring
     browse-at-remote
     bind-key
     benchmark-init
     avy
     auto-compile
     anzu
     aggressive-indent
     ag
     )))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

(provide 'selected-packages)
