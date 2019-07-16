;;; For emacs version >=27, introduce 'early-init.el', which is run before user-init-file.
;;; load before package and UI initialization.

;; UI config, Faster to disable when in emacs27
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (and (not (eq system-type 'darwin)) (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
