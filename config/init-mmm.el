;;; init-mmm.el --- Multiple major modes support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'mmm-mode)
(require 'mmm-auto)

(setq mmm-global-mode 'buffers-with-submode-classes)
(setq mmm-submode-decoration-level 2)

(provide 'init-mmm)
;;; init-mmm.el ends here
