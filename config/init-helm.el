(require 'helm)

(require 'helm-config)
(require 'helm-grep)
(require 'helm-swoop)

(helm-mode 1)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p              t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source        t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp           t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                       4 ; scroll 4 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf    t
      helm-echo-input-in-header-line           t
      helm-move-to-line-cycle-in-source        t ; move to end or beginning of source when reaching top or bottom of source.
      helm-buffer-skip-remote-checking         t
      helm-mode-fuzzy-match                    t
      helm-buffers-fuzzy-matching              t ; fuzzy matching buffer names when non-nil
                                        ; useful in helm-mini that lists buffers
      helm-org-headings-fontify                t
      helm-semantic-fuzzy-match                t
      helm-imenu-fuzzy-match                   t
      helm-locate-fuzzy-match                  t
      helm-apropos-fuzzy-match                 t
      helm-lisp-fuzzy-completion               t
      helm-display-header-line                 nil)

;; live grep in helm
(when (executable-find "ack-grep")
  (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))

(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

(after-load 'helm
  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))


  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)  ; rebihnd tab to do persistent action
  (define-key helm-map (kbd "C-i")   'helm-execute-persistent-action)    ; make TAB works in terminal
  (define-key helm-map (kbd "C-z")   'helm-select-action)               ; list actions using C-z

  (define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
  (define-key helm-grep-mode-map (kbd "n")         'helm-grep-mode-jump-other-window-forward)
  (define-key helm-grep-mode-map (kbd "p")         'helm-grep-mode-jump-other-window-backward)

  (define-key helm-command-map (kbd "o")     'helm-occur)
  (define-key helm-command-map (kbd "g")     'helm-do-grep)
  (define-key helm-command-map (kbd "C-c w") 'helm-wikipedia-suggest)
  (define-key helm-command-map (kbd "SPC")   'helm-all-mark-rings)


  (defun lew/helm-hide-minibuffer-maybe ()
    "Hide minibuffer in Helm session if we use the header line as input field."
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face
                     (let ((bg-color (face-background 'default nil)))
                       `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil))))


  (add-hook 'helm-minibuffer-set-up-hook
            'lew/helm-hide-minibuffer-maybe)

  ;; Save curent position to mark ring
  (add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)
  
  (defun lew/helm-rgrep ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'helm-do-grep-ag))))


;; helm-swoop
(setq helm-multi-swoop-edit-save t
      helm-swoop-split-with-multiple-windows nil
      helm-swoop-split-direction 'split-window-vertically
      helm-swoop-speed-or-color nil
      helm-swoop-move-to-line-cycle t
      helm-swoop-use-line-number-face t
      helm-swoop-use-fuzzy-match t)

(after-load 'helm-swoop
  (define-key helm-command-map (kbd "s") 'helm-swoop)
  (define-key helm-command-map (kbd "r") 'helm-swoop-back-to-last-point)
  (define-key helm-command-map (kbd "m") 'helm-multi-swoop)
  (define-key helm-command-map (kbd "M") 'helm-multi-swoop-all))

;; taken from full-ack.el
(defvar lew/project-root-file-patterns
  '(".project\\'" ".xcodeproj\\'" ".sln\\'" "\\`Project.ede\\'"
    "\\`.git\\'" "\\`.bzr\\'" "\\`_darcs\\'" "\\`.hg\\'"))

(defun lew/guess-project-root ()
  (interactive)
  (catch 'root
    (let ((dir (expand-file-name (if buffer-file-name
                                     (file-name-directory buffer-file-name)
                                   default-directory)))
          (prev-dir nil)
          (pattern (mapconcat 'identity lew/project-root-file-patterns "\\|")))
      (while (not (equal dir prev-dir))
        (when (directory-files dir nil pattern t)
          (throw 'root dir))
        (setq prev-dir dir
              dir (file-name-directory (directory-file-name dir)))))))



(provide 'init-helm)
