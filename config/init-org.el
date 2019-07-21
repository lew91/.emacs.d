;;; init-org.el --- basic org-mode configuration -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;;一Some basic settings，Can adapt to most situations
;;;
;;; Code:

(when (featurep 'cocoa)
  (require 'grab-mac-link))
(require 'org-cliplink)
(require 'hl-todo)
(require 'writeroom-mode)
(require 'org-pomodoro)
(require 'org-bullets)
(require 'cal-china-x)
(require 'ox-md)                ; Markdown back-end
(require 'ox-jekyll-md) ; org files -> jekyll style markdown files
(require 'ox-latex)


;; Various preferences
(setq org-log-done 'time                 ; record the time that a todo was archived
      org-startup-indented t
      org-edit-timestamp-down-means-later t
      org-hide-emphasis-markers t
      org-catch-invisible-edits 'show
      org-export-coding-system 'utf-8
      org-fast-tag-selection-single-key 'expert
      org-html-validation-link nil
      org-export-kill-product-buffer-when-displayed t
      org-tags-column 80)

(setq org-refile-use-cache nil)
(setq org-support-shift-select t)

(setq org-capture-templates
      `(("t" "todo" entry (file "")  ; "" => `org-default-notes-file'
         "* NEXT %?\n%U\n" :clock-resume t)
        ("n" "note" entry (file "")
         "* %? :NOTE:\n%U\n%a\n" :clock-resume t)
        ))

;;; LaTex
;; Compiling latex documents with unicode symbols
;; #+latex_compiler: xelatex
;; #+latex_header: \usepackage{libertine}
;; #+latex_header: \usepackage{unicode-math}
(setq org-latex-inputenc-alist '(("utf8" . "utf8x")))
(setq org-latex-default-packages-alist (cons '("mathletters" "ucs" nil) org-latex-default-packages-alist))

(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))


;;; Org clock

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(with-eval-after-load 'org
  (org-clock-persistence-insinuate))
(setq org-clock-persist t)
(setq org-clock-in-resume t)

;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Save state changes in the LOGBOOK drawer
(setq org-log-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Show clock sums as hours and minutes, not "n days" etc.
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

;;; Show the clocked-in task - if any - in the header line
(defun jl/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun jl/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))

(add-hook 'org-clock-in-hook 'jl/show-org-clock-in-header-line)
(add-hook 'org-clock-out-hook 'jl/hide-org-clock-from-header-line)
(add-hook 'org-clock-cancel-hook 'jl/hide-org-clock-from-header-line)

(with-eval-after-load 'org-clock
  (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
  (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu))

;;; Archiving

;;(setq org-archive-mark-done nil)
;;(setq org-archive-location "%s_archive::* Archive")


(setq org-pomodoro-keep-killed-pomodoro-time t)
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro))


(add-hook 'org-mode-hook (lambda () (org-bullets-mode)))
(add-hook 'org-agenda-mode-hook 'hl-line-mode)

(dolist (hook (list
               'prog-mode-hook
               'org-mode-hook
               'markdown-mode-hook
               ))
  (add-hook hook '(lambda ()
                    (hl-todo-mode))))



;; Lots of stuff from http://doc.norang.ca/org-mode.html
;; TODO: fail gracefully
(defun jl/grab-ditaa (url jar-name)
  "Download URL and extract JAR-NAME as `org-ditaa-jar-path'."
  ;; TODO: handle errors
  (message "Grabbing %s for org." jar-name)
  (let ((zip-temp (make-temp-name "emacs-ditaa")))
    (unwind-protect
        (progn
          (when (executable-find "unzip")
            (url-copy-file url zip-temp)
            (shell-command (concat "unzip -p " (shell-quote-argument zip-temp)
                                   " " (shell-quote-argument jar-name) " > "
                                   (shell-quote-argument org-ditaa-jar-path)))))
      (when (file-exists-p zip-temp)
        (delete-file zip-temp)))))

(with-eval-after-load 'ob-ditaa
  (unless (and (boundp 'org-ditaa-jar-path)
               (file-exists-p org-ditaa-jar-path))
    (let ((jar-name "ditaa0_9.jar")
          (url "http://jaist.dl.sourceforge.net/project/ditaa/ditaa/0.9/ditaa0_9.zip"))
      (setq org-ditaa-jar-path (expand-file-name jar-name (file-name-directory user-init-file)))
      (unless (file-exists-p org-ditaa-jar-path)
        (jl/grab-ditaa url jar-name)))))

(with-eval-after-load 'ob-plantuml
  (let ((jar-name "plantuml.jar")
        (url "http://jaist.dl.sourceforge.net/project/plantuml/plantuml.jar"))
    (setq org-plantuml-jar-path (expand-file-name jar-name (file-name-directory user-init-file)))
    (unless (file-exists-p org-plantuml-jar-path)
      (url-copy-file url org-plantuml-jar-path))))


;; Re-align tags when window shape changes
(with-eval-after-load 'org-agenda
  (add-hook 'org-agenda-mode-hook
            (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t))))



;; Re-align tags when window shape changes
(with-eval-after-load 'org-agenda
  (add-hook 'org-agenda-mode-hook
            (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t))))



(define-minor-mode prose-mode
  "Set up a buffer for prose editing.
This enables or modifies a number of settings so that the
experience of editing prose is a little more like that of a
typical word processor."
  nil " Prose" nil
  (if prose-mode
      (progn
        (when (fboundp 'writeroom-mode)
          (writeroom-mode 1))
        (setq truncate-lines nil)
        (setq word-wrap t)
        (setq cursor-type 'bar)
        (when (eq major-mode 'org)
          (kill-local-variable 'buffer-face-mode-face))
        (buffer-face-mode 1)
        ;;(delete-selection-mode 1)
        (setq-local blink-cursor-interval 0.6)
        (setq-local show-trailing-whitespace nil)
        (setq-local line-spacing 0.2)
        (setq-local electric-pair-mode nil)
        (ignore-errors (flyspell-mode 1))
        (visual-line-mode 1))
    (kill-local-variable 'truncate-lines)
    (kill-local-variable 'word-wrap)
    (kill-local-variable 'cursor-type)
    (kill-local-variable 'blink-cursor-interval)
    (kill-local-variable 'show-trailing-whitespace)
    (kill-local-variable 'line-spacing)
    (kill-local-variable 'electric-pair-mode)
    (buffer-face-mode -1)
    ;; (delete-selection-mode -1)
    (flyspell-mode -1)
    (visual-line-mode -1)
    (when (fboundp 'writeroom-mode)
      (writeroom-mode 0))))

;;(add-hook 'org-mode-hook 'buffer-face-mode)



;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))

(with-eval-after-load 'org-agenda
  (add-to-list 'org-agenda-after-show-hook 'org-show-entry))

(advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))

;; Exclude DONE state tasks from refile targets
(defun jl/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'jl/verify-refile-target)

(defun jl/org-refile-anywhere (&optional goto default-buffer rfloc msg)
  "A version of `org-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-refile goto default-buffer rfloc msg)))

(defun jl/org-agenda-refile-anywhere (&optional goto rfloc no-update)
  "A version of `org-agenda-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-agenda-refile goto rfloc no-update)))

;; Targets start with the file name - allows creating level 1 tasks
;;(setq org-refile-use-outline-path (quote file))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)


;;; To-do settings

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
              (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))
      org-todo-repeat-to-state "NEXT")

(setq org-todo-keyword-faces
      (quote (("NEXT" :inherit warning)
              ("PROJECT" :inherit font-lock-string-face))))


;;; Agenda views

(setq-default org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))


(let ((active-project-match "-INBOX/PROJECT"))

  (setq org-stuck-projects
        `(,active-project-match ("NEXT")))

  (setq org-agenda-compact-blocks t
        org-agenda-sticky t
        org-agenda-start-on-weekday nil
        org-agenda-span 'day
        org-agenda-include-diary nil
        org-agenda-sorting-strategy
        '((agenda habit-down time-up user-defined-up effort-up category-keep)
          (todo category-up effort-up)
          (tags category-up effort-up)
          (search category-up))
        org-agenda-window-setup 'current-window
        org-agenda-custom-commands
        `(("N" "Notes" tags "NOTE"
           ((org-agenda-overriding-header "Notes")
            (org-tags-match-list-sublevels t)))
          ("g" "GTD"
           ((agenda "" nil)
            (tags "INBOX"
                  ((org-agenda-overriding-header "Inbox")
                   (org-tags-match-list-sublevels nil)))
            (stuck ""
                   ((org-agenda-overriding-header "Stuck Projects")
                    (org-agenda-tags-todo-honor-ignore-options t)
                    (org-tags-match-list-sublevels t)
                    (org-agenda-todo-ignore-scheduled 'future)))
            (tags-todo "-INBOX"
                       ((org-agenda-overriding-header "Next Actions")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("HOLD" "WAITING"))
                                (org-agenda-skip-entry-if 'nottodo '("NEXT")))))
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(todo-state-down effort-up category-keep))))
            (tags-todo ,active-project-match
                       ((org-agenda-overriding-header "Projects")
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-INBOX/-NEXT"
                       ((org-agenda-overriding-header "Orphaned Tasks")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("PROJECT" "HOLD" "WAITING" "DELEGATED"))
                                (org-agenda-skip-subtree-if 'nottododo '("TODO")))))
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "/WAITING"
                       ((org-agenda-overriding-header "Waiting")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "/DELEGATED"
                       ((org-agenda-overriding-header "Delegated")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-INBOX"
                       ((org-agenda-overriding-header "On Hold")
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("WAITING"))
                                (org-agenda-skip-entry-if 'nottodo '("HOLD")))))
                        (org-tags-match-list-sublevels nil)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            ;; (tags-todo "-NEXT"
            ;;            ((org-agenda-overriding-header "All other TODOs")
            ;;             (org-match-list-sublevels t)))
            )))))

(defun chinese-calender (&optional args)
  "Open Chinese Lunar calenadar."
  (interactive "P")
  (let* ((calendar-date-display-form
          '((cal-china-x-calendar-display-form
             (mapcar (lambda (el) (string-to-number el))
                     (list month day year)))))
         (diary-date-forms chinese-date-diary-pattern)

         ;; chinese month and year
         (calendar-font-lock-keywords
          (append calendar-font-lock-keywords
                  '(("[0-9]+年\\ *[0-9]+月" . font-lock-function-name-face))))

         (calendar-chinese-celestial-stem cal-china-x-celestial-stem)
         (calendar-chinese-terrestrial-branch cal-china-x-terrestrial-branch)
         (calendar-month-header '(propertize (format "%d年%2d月" year month)
                                             'font-lock-face
                                             'calendar-month-header))

         ;; if chinese font width equals to twice of ascii font
         (calendar-day-header-array cal-china-x-days)

         (calendar-mode-line-format
          (list
           (calendar-mode-line-entry 'calendar-scroll-right "previous month" "<")
           "Calendar"

           '(cal-china-x-get-holiday date)

           '(concat " " (calendar-date-string date t)
                    (format " 第%d周"
                            (funcall (if cal-china-x-custom-week-start-date
                                         'cal-china-x-custom-week-of-date
                                       'cal-china-x-week-of-date)
                                     date)))

           '(cal-china-x-chinese-date-string date)

           ;; (concat
           ;;  (calendar-mode-line-entry 'calendar-goto-info-node "read Info on Calendar"
           ;;                            nil "info")
           ;;  " / "
           ;;  (calendar-mode-line-entry 'calendar-other-month "choose another month"
           ;;                            nil "other")
           ;;  " / "
           ;;  (calendar-mode-line-entry 'calendar-goto-today "go to today's date"
           ;;                            nil "today"))

           (calendar-mode-line-entry 'calendar-scroll-left "next month" ">")))

         other-holidays
         (mark-holidays-in-calendar t)
         (cal-china-x-important-holidays cal-china-x-chinese-holidays)
         (cal-china-x-general-holidays '((holiday-lunar 1 15 "元宵节")))
         (calendar-holidays
          (append cal-china-x-important-holidays
                  cal-china-x-general-holidays
                  other-holidays)))
    (advice-add 'calendar-mark-holidays :around #'cal-china-x-mark-holidays)
    (calendar args)))

(defadvice calendar-exit (before calendar-exit-before-hack activate)
  "Clean the cal-chinese-x setup."
  (advice-remove 'calendar-mark-holidays #'cal-china-x-mark-holidays))

(with-eval-after-load 'org

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


   (define-key org-mode-map (kbd "<") '(lambda () (interactive)
                                       (if (or (region-active-p) ( looking-back "^\s*" 1))
                                           (hydra-org-template/body)
                                         (self-insert-command 1))))

  (require 'os-md nil t)
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
      (ad-set-arg 1 fixed-contents))))



;; org babel settings
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   `((R . t)
     (ditaa . t)
     (dot . t)
     (emacs-lisp . t)
     (haskell . nil)
     (latex . t)
     (ledger . t)
     (ocaml . nil)
     (octave . t)
     (plantuml . t)
     (python . t)
     (ruby . t)
     (screen . nil)
     (,(if (locate-library "ob-sh") 'sh 'shell) . t)
     (sql . t)
     (sqlite . t))))






(provide 'init-org)
;;; init-org.el ends here
