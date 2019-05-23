(require 'setup-package)

;; install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   '(
     flycheck-ycmd company-ycmd smooth-scrolling slime-company slime format-all visual-regexp benchmark-init osx-trash undo-tree ivy-xref counsel ivy hungry-delete magit flycheck-popup-tip hl-todo flycheck-pos-tip yasnippet-snippets ag rg wgrep-ag paradox highlight-parentheses yaml-mode writeroom-mode whole-line-or-region whitespace-cleanup-mode wgrep vlf vc-darcs uptimes unfill textile-mode symbol-overlay switch-window sqlformat spacemacs-theme smex session scratch regex-tool rainbow-mode rainbow-delimiters pip-requirements paredit-everywhere page-break-lines osx-location origami org-pomodoro org-cliplink org-bullets ns-auto-titlebar multiple-cursors move-dup mode-line-bell mmm-mode markdown-mode macrostep list-unicode-display ledger-mode ipretty info-colors immortal-scratch ibuffer-vc ibuffer-projectile hydra highlight-quoted highlight-escape-sequences helm-swoop helm-projectile helm-descbinds helm-ag guide-key grab-mac-link goto-line-preview gnuplot fullframe flycheck-package flycheck-ledger flycheck-color-mode-line expand-region exec-path-from-shell elisp-slime-nav dotenv-mode disable-mouse diredfl dimmer diminish diff-hl default-text-scale dash-at-point darcsum daemons company-quickhelp company-anaconda command-log-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized cmd-to-echo cl-libify cl-lib-highlight browse-kill-ring browse-at-remote bind-key avy auto-compile anzu aggressive-indent

     )))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

(provide 'selected-packages)
