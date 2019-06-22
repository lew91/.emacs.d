;;; init-cmake.el --- cmake-mode for C/C++ projects -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; For microcontroller programming, there are three commonly used commands: make, make flash and make clean.
;;; make is used to build the project and generate the .hex file, make flash is used to load the .hex file to
;;; the microcontroller and make clean is used to clean all generated files.
;;;
;;; Use the command 'cd ../', because the makefile is on the top of source files and header files.

;;; Code:

(require 'cmake-mode)
(setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode))
       '(("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))

(defun lew/company-cmake-setup ()
  (add-to-list 'company-backends 'company-cmake))
(add-hook 'cmake-mode-hook 'lew/company-cmake-setup)

(defun cc-mode-compile-make ()
  (interactive)
  (setq compile-command "cd ../ && make")
  (call-interactively 'compile))

(defun cc-mode-compile-flash ()
  (interactive)
  (setq compile-command "cd ../ && make flash")
  (call-interactively 'compile))

(defun cc-mode-compile-clean ()
  (interactive)
  (setq compile-command "cd ../ && make clean")
  (call-interactively 'coompile))

(defun cc-mode-compile ()
  (local-set-key (kbd "C-x C m") 'cc-mode-compile-make)
  (local-set-key (kbd "C-x C f") 'cc-mode-compile-flash)
  (local-set-key (kbd "c-x C c") 'cc-mode-compile-clean))

(add-hook 'c-mode-hook 'cc-mode-compile)
(add-hook 'c++-mode-hook 'cc-mode-compile)

(provide 'init-cmake)
;;; init-cmake.el ends here
