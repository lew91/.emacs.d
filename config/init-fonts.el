(let ((emacs-font-size 13)
      emacs-font-name)
  (cond
   ((featurep 'cocoa)
    (setq emacs-font-name "Monaco"))
   ((string-equal system-type "gnu/linux")
    (setq emacs-font-name "WenQuanYi Micro Hei Mono")))
  (when (display-grayscale-p)
    (set-frame-font (format "%s-%s" (eval emacs-font-name) (eval emacs-font-size)))
    (set-fontset-font (frame-parameter nil 'font) 'unicode (eval emacs-font-name))))

(with-eval-after-load 'org
  (defun org-buffer-face-mode-variable()
    (interactive)
    (make-face 'width-font-face)
    (set-face-attribute 'width-font-face nil :font "Sarasa Mono SC 14")
    (setq buffer-face-mode-face 'width-font-face)
    (buffer-face-mode))

  (add-hook 'org-mode-hook 'org-buffer-face-mode-variable))

  

;; (progn
;;   ;; set a default font
;;   (cond
;;    ((eq system-type 'darwin)
;;     (when (member "Monaco" (font-family-list))
;;       (set-face-attribute 'default nil :font "Monaco" :height 140))
;;     ;; specify font for Chinese characters
;;     (when (member "WenQuanYi Zen Hei" (font-family-list))
;;       (set-fontset-font t '(#x4e00 . #x9fff) "WenQuanYi Zen Hei"))
;;     ;; change scale, so that Chinese characters width = 2 * English characters width
;;     (setq face-font-rescale-alist '(("Monaco" . 1.0) ("WenQuanYi Zen Hei" . 1.2)))
;;     )

;;    ((eq system-type 'windows-nt)
;;     nil)

;;    ((eq system-type 'gnu/linux)
;;     (when (member "DejaVu Sans Mono" (font-family-list))
;;       (set-face-attribute 'default :font  "Dejavu Sans Mono" ))
;;     (when (member "WenQuanYi Micro Hei" (font-family-list))
;;       (set-fontset-font t '(#x4e00 . #x9fff) "WenQuanYi Micro Hei"))
;;     )

;;    ;; specify font for all unicode characters
;;    ((when (member "Apple Color Emoji" (font-family-list)))
;;     (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))
;;    ((when (member "Symbola" (font-family-list)))
;;     (set-fontset-font 'unicode "Symbola" nil 'prepend))

;;    )
;;   )

(provide 'init-fonts)
;; init-fonts.el ends here
