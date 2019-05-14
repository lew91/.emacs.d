(require 'helm)

(require 'helm-config)
(require 'helm-swoop)

(helm-mode 1)

(after-load 'helm
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t
        helm-echo-input-in-header-line t)



  ;;-------------------------------------------------------
  ;; key bindings
  ;;---------------------------------------------------
  ;;(global-set-key (kbd "M-x") 'helm-M-x)
  ;;(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
  ;; (global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
  ;; (global-set-key (kbd "C-x C-f") 'helm-find-files)
  ;; (global-set-key (kbd "M-y") 'helm-show-kill-ring)

  ;; ;; helm-mini
  ;; (global-set-key (kbd "C-x b") 'helm-mini)
  ;; (setq helm-buffers-fuzzy-matching t
  ;;       helm-recentf-fuzzy-match    t)

  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))

  (define-key helm-command-map (kbd "o")     'helm-occur)
  (define-key helm-command-map (kbd "g")     'helm-do-grep)
  (define-key helm-command-map (kbd "C-c w") 'helm-wikipedia-suggest)
  (define-key helm-command-map (kbd "SPC")   'helm-all-mark-rings)




  (defun jakelew//helm-hide-minibuffer-maybe ()
    "Hide minibuffer in Helm session if we use the header line as input field."
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face
                     (let ((bg-color (face-background 'default nil)))
                       `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil))))


  (add-hook 'helm-minibuffer-set-up-hook
            'jakelew//helm-hide-minibuffer-maybe)


  (defun pl/helm-alive-p ()
    (if (boundp 'helm-alive-p)
        (symbol-value 'helm-alive-p)))

  ;;(add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)

  ;; live grep in helm
  (when (executable-find "ack-grep")
    (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
          helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))

  (setq helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match    t)

  ;;(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

  (setq helm-locate-fuzzy-match t)
  (setq helm-apropos-fuzzy-match t)
  (setq helm-lisp-fuzzy-completion t)

  )

;;------------------------------------------------------------------
;; projcetile helm 或许可以用ibuffer projectile
;;---------------------------------------------------------------
;; (require 'helm-projectile)
;; (setq projectile-completion-system 'helm)

;; Change the keybinds to whatever you like :)
;; (global-set-key (kbd "C-c h s") 'helm-swoop)
;; (global-set-key (kbd "C-c h r") 'helm-swoop-back-to-last-point)
;; (global-set-key (kbd "C-c h m") 'helm-multi-swoop)
;; (global-set-key (kbd "C-c h M") 'helm-multi-swoop-all)


;; helm-swoop
(after-load 'helm-swoop
  (setq helm-multi-swoop-edit-save t
        helm-swoop-split-with-multiple-windows nil
        helm-swoop-split-direction 'split-window-vertically
        helm-swoop-speed-or-color nil
        helm-swoop-move-to-line-cycle t
        helm-swoop-use-line-number-face t
        helm-swoop-use-fuzzy-match t)

  (define-key helm-command-map (kbd "s") 'helm-swoop)
  (define-key helm-command-map (kbd "r") 'helm-swoop-back-to-last-point)
  (define-key helm-command-map (kbd "m") 'helm-multi-swoop)
  (define-key helm-command-map (kbd "M") 'helm-multi-swoop-all)
  )






(provide 'init-helm)
