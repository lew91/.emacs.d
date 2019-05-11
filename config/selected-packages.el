(require 'setup-package)

;; install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   '(
     anzu
     auto-compile
     avy
     bind-key
     company
     company-anaconda
     company-quickhelp
     dash
     dash-at-point
     dash-docs
     diminish
     diff-hl
     elisp-slime-nav
     exec-path-from-shell
     expand-region
     epl
     f
     flx
     flx-ido
     flycheck
     flycheck-pos-tip
     guide-key
     gh-md
     gnuplot
     helm
     helm-ag
     helm-flx
     helm-projectile
     helm-swoop
     highlight-escape-sequences
     hydra
     markdown-mode
     multiple-cursors
     org-brain
     org-bullets
     org-pomodoro
     osx-location
     osx-clipboard
     osx-trash
     page-break-lines
     paredit
     paredit-everywhere
     popup
     popwin
     pos-tip
     projectile
     smartparens
     pythonic
     rainbow-delimiters
     reveal-in-osx-finder
     s
     session
     spacemacs-theme
     switch-window
     symbol-overlay
     yasnippet
     yasnippet-snippets
     whitespace-cleanup-mode
     )))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

(provide 'selected-packages)
