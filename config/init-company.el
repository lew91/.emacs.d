(require 'company)
;;(require 'company-quickhelp)
(require 'company-math)
(require 'company-tabnine)


;; Config for company mode.
(global-company-mode)
(diminish 'company-mode)
(setq company-idle-delay 0.2)   ; set the completion menu pop-up delay
(setq company-minimum-prefix-length 1) ; pop up a completion menu by tapping a character
(setq company-show-numbers nil)   ; do not display numbers on the left
(setq company-require-match nil) ; allow input string that do not match candidate words

(with-eval-after-load 'company
  (dolist (backend '(company-eclim company-semantic))
    (delq backend company-backends)))

(define-key company-mode-map (kbd "M-/") 'company-complete)
(define-key company-active-map (kbd "M-/") 'company-other-backend)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
;;(define-key company-active-map (kbd "M-h") 'company-quickhelp-manual-begin)
(setq-default company-dabbrev-other-buffers 'all
             company-tooltip-align-annotations t)
(global-set-key (kbd "M-C-/") 'company-complete)

(setq company-quickhelp-delay nil)  ;; manually popup
(add-hook 'company-mode-hook 'company-quickhelp-mode)
(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-c h") 'company-quickhelp-manual-begin))



;; Suspend page-break-lines-mode while company menu is active
;; (see https://github.com/company-mode/company-mode/issues/416)
(with-eval-after-load 'company
  (with-eval-after-load 'page-break-lines
    (defvar-local jl/page-break-lines-on-p nil)

    (defun jl/page-break-lines-disable (&rest ignore)
      (when (setq jl/page-break-lines-on-p (bound-and-true-p page-break-lines-mode))
        (page-break-lines-mode -1)))

    (defun jl/page-break-lines-maybe-reenable (&rest ignore)
      (when jl/page-break-lines-on-p
        (page-break-lines-mode 1)))

    (add-hook 'company-completion-started-hook 'jl/page-break-lines-disable)
    (add-hook 'company-after-completion-hook 'jl/page-break-lines-maybe-reenable)))


;;TabNine
(add-to-list 'company-backends 'company-tabnine)


;; Add yasnippet support for all company backends.
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))
(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))


;; Company math
(add-to-list 'company-backends 'company-math-symbols-unicode)
(add-to-list 'company-backends 'company-math-symbols-latex)


(provide 'init-company)
