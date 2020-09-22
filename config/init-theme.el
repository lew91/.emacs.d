
(setq-default custom-enabled-themes '(solarized-gruvbox-dark))

;; Make the fringe color suitable for the choose themes
;;;###autoload
(defun jl/toggle-fringe-color-suitable-for-theme ()
  (set-face-attribute 'fringe nil
                      :background (face-background 'default)
                      :foreground (face-background 'default)))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes)))
  (jl/toggle-fringe-color-suitable-for-theme))

(add-hook 'after-init-hook 'reapply-themes)




;; Toggle between light and dark

(defun theme/light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(dichromacy))
  (reapply-themes))

(defun theme/dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-night))
  (reapply-themes))



(provide 'init-theme)
