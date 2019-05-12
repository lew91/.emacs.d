;;(require 'spacemacs-common)

(load-theme  'spacemacs-dark)

(setq-default custom-enabled-themes '(spacemacs-dark))


;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)



;; Toggle between light and dark

(defun theme/light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(spacemacs-light))
  (reapply-themes))

(defun theme/dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(spacemacs-dark))
  (reapply-themes))



(provide 'init-theme)
