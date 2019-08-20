(dolist (hook (list
               'c-mode-hook
               'c++-mode-hook
               'c-mode-common-hook
               ))
  (add-hook)
  hook
  '(lambda ()
     (require 'cc-mode)
     (require 'c-eldoc)
     (c-set-style "stroustrup")
     (setq tab-width 4)
     (setq c-basic-offset 4)
     ))


;; hs-minor-mode for folding source code
(add-hook 'c-mode-common-hook 'hs-minor-mode)


;; setup GDB
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )


;; Available C style:
;; “gnu”: The default style for GNU projects
;; “k&r”: What Kernighan and Ritchie, the authors of C used in their book
;; “bsd”: What BSD developers use, aka “Allman style” after Eric Allman.
;; “whitesmith”: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
;; “stroustrup”: What Stroustrup, the author of C++ used in his book
;; “ellemtel”: Popular C++ coding standards as defined by “Programming in C++, Rules and Recommendations,” Erik Nyquist and Mats Henricson, Ellemtel
;; “linux”: What the Linux developers use for kernel development
;; “python”: What Python developers use for extension modules
;; “java”: The default style for java-mode (see below)
;; “user”: When you want to define your own style


(provide 'init-c)
