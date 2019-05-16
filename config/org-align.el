;;; org-align.el --- org-mode enhanced settings，Chinese support -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; Use hrdira to enhance shortcuts，Chinese and English font alignment settings
;;;
;;; Code:




(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode)))


;; (setq org-startup-indented t
;;           org-ellipsis (if (char-displayable-p ?) "  " nil)
;;           org-pretty-entities t
;;           org-hide-emphasis-markers t
;;           )


(after-load 'org

  ;;(require 'org-tempo) ; Require from org 9 on-wards for old template expansion
  ;; Reset the org-template expnsion system, this is need after upgrading to org 9 for some reason
  (setq org-structure-template-alist (eval (car (get 'org-structure-template-alist 'standard-value))))

  (eval-and-compile
    (defun hot-expand (str &optional mod header)
      "Expand org template.
STR is a structure template string recognized by org like <s. MOD is a
string with additional parameters to add the begin line of the
structure element. HEADER string includes more parameters that are
prepended to the element after the #+HEADER: tag."
      (let (text)
        (when (region-active-p)
          (setq text (buffer-substring (region-beginning) (region-end)))
          (delete-region (region-beginning) (region-end)))
        (when header (insert "#+HEADER: " header) (forward-line))
        (insert str)
        (org-try-structure-completion)
        ;;(org-tempo-complete-tag)
        (when mod (insert mod) (forward-line))
        (when text (insert text)))))

  (defhydra hydra-org-template (:color blue :hint nil)
    "
_c_enter  qu_o_te     _e_macs-lisp    _L_aTeX:
_l_atex   _E_xample   p_y_thon        _i_ndex:
_a_scii   _v_erse     ip_Y_thon       _I_NCLUDE:
_s_rc     _g_o        _r_uby          _H_TML:
_h_tml    _S_HELL     _p_erl          _A_SCII:
^ ^       ^ ^         _P_erl tangled  plant_u_ml
"
    ("s" (hot-expand "<s"))
    ("E" (hot-expand "<e"))
    ("o" (hot-expand "<q"))
    ("v" (hot-expand "<v"))
    ("c" (hot-expand "<c"))
    ("l" (hot-expand "<l"))
    ("h" (hot-expand "<h"))
    ("a" (hot-expand "<a"))
    ("L" (hot-expand "<L"))
    ("i" (hot-expand "<i"))
    ("e" (hot-expand "<s" "emacs-lisp"))
    ("y" (hot-expand "<s" "python :results output"))
    ("Y" (hot-expand "<s" "ipython :session :exports both :results raw drawer\n$0"))
    ("g" (hot-expand "<s" "go :imports '\(\"fmt\"\)"))
    ("p" (hot-expand "<s" "perl"))
    ("r" (hot-expand "<s" "ruby"))
    ("S" (hot-expand "<s" "sh"))
    ("u" (hot-expand "<s" "plantuml :file CHANGE.png"))
    ("P" (progn
           (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
           (hot-expand "<s" "perl")))
    ("I" (hot-expand "<I"))
    ("H" (hot-expand "<H"))
    ("A" (hot-expand "<A"))
    ("<" self-insert-command "ins")
    ("q" nil "quit"))


  (bind-key "<"
            (lambda () (interactive)
              (if (or (region-active-p) (looking-back "^\s*" 1))
                  (hydra-org-template/body)
                (self-insert-command 1)))
            org-mode-map)

  (require 'os-md nil t)
  ;; copy from Chinese layer
  (defadvice org-html-paragraph (before fsh-org-html-paragraph-advice
                                        (paragraph contents info) activate)
    "Join consecutive Chinese lines into a single long line without unwanted space when exporting 'org-mode' to html."
    (let ((fixed-contents)
          (orig-contents (ad-get-arg 1))
          (reg-han "[[:multibyte:]]"))
      (setq fixed-contents (replace-regexp-in-string
                            ;; (concat "\\(" reg-han "\\) *\n *\\(" reg-han "\\)")
                            (concat "\\(" reg-han "\\) *\n *")
                            "\\1" orig-contents))
      (ad-set-arg 1 fixed-contents)))



  ;; org-mode 自动换行
  (add-hook 'org-mode-hook 'toggle-truncate-lines)


  ;;中文与英文对齐
  (defun jakelew/set-font (english chinese english-size chinese-size)
    (set-face-attribute 'default nil :font
                        (format   "%s:pixelsize=%d"  english english-size))
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
                        (font-spec :family chinese :size chinese-size))))

  ;; 不同系统用不同字体
  (cond ((featurep 'cocoa)
         (jakelew/set-font "Source Code Pro" "Hiragino Sans GB" 14 16)
         )
        ((eq system-type 'windows-nt)
         (jakelew/set-font "Source Code Pro" "WenQuanYi Zen Hei Mono" 14 16))
        )

  )


(provide 'org-align)
;;; jakelew-org.el ends here
