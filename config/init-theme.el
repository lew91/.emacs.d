(setq-default custom-enabled-themes '(wombat))

;; Make the fringe color suitable for the choose themes
;;;###autoload
(defun lew/toggle-fringe-color-suitable-for-theme ()
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
  (lew/toggle-fringe-color-suitable-for-theme))

(add-hook 'after-init-hook 'reapply-themes)



;; Toggle between light and dark

(defun theme/light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(adwaita))
  (reapply-themes))

(defun theme/dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(wombat))
  (reapply-themes))



(provide 'init-theme)
