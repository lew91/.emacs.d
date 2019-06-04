
(progn
  ;; set a default font
  (cond
   ((eq system-type 'darwin)
    (when (member "Source Code Pro" (font-family-list))
      (set-face-attribute 'default nil :font "Source Code Pro" :height 140))
    ;; specify font for Chinese characters
    (when (member "WenQuanYi Zen Hei" (font-family-list))
      (set-fontset-font t '(#x4e00 . #x9fff) "WenQuanYi Zen Hei"))
    ;; change scale, so that Chinese characters width = 2 * English characters width
    (setq face-font-rescale-alist '(("Source Code Pro" . 1.0) ("WenQuanYi Zen Hei" . 1.2)))
    )

   ((eq system-type 'windows-nt)
    nil)

   ((eq system-type 'gnu/linux)
    (when (member "DejaVu Sans Mono" (font-family-list))
      (set-face-attribute 'default :font  "Dejavu Sans Mono" ))
    (when (member "WenQuanYi Micro Hei" (font-family-list))
      (set-fontset-font t '(#x4e00 . #x9fff) "WenQuanYi Micro Hei"))
    )

   ;; specify font for all unicode characters
   ((when (member "Apple Color Emoji" (font-family-list)))
    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))
   ((when (member "Symbola" (font-family-list)))
    (set-fontset-font 'unicode "Symbola" nil 'prepend))

   )
  )

(provide 'init-fonts)
;; init-fonts.el ends here
