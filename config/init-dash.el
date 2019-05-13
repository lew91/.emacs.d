(when (featurep 'cocoa)
  (require 'dash-at-point)
  (global-set-key (kbd "C-c D") 'dash-at-point)
  )

(provide 'init-dash)
