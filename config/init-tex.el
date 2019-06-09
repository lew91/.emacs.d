(require 'cdlatex)
(require 'reftex)

(load "auctex.el" nil t t)


(setq TeX-parse-self t ; Parse documents to provide completion for packages, etc
      TeX-auto-save t  ; Automatically save style information
      TeX-electric-sub-and-superscript t  ; Automatically insert braces after sub- and superscript in math mode
      TeX-electric-math '("\\(" "\\)")
      ;; Don't insert magic quotes right away
      TeX-quote-after-quote t
      ;; Don't ask for confirmation when cleaning
      TeX-clean-confirm nil
      ;; Provide foreground and inverse search with SyncTex
      TeX-source-correlate-map t
      TeX-source-correlate-method 'synctex)

(setq-default TeX-master nil       ; Ask for the mater file
              TeX-engine 'luatex   ; Use a Modern engine
              ;; redundant in 11.88, but keep for older AUCtex
              TeX-PDF-mode t)

(add-hook 'bibtex-mode-hook (lambda () (run-hooks 'prog-mode-hook)))
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)


(setq reftex-plug-into-AUCTeX t
        ;; Automatically derive labels, and prompt for confirmation
        reftex-insert-label-flags '(t t)
        reftex-label-alist
        '(
          ;; Additional label definitions for RefTeX.
          ("definition" ?d "def:" "~\\ref{%s}"
           lunaryorn-reftex-find-ams-environment-caption
           ("definition" "def.") -3)
          ("theorem" ?h "thm:" "~\\ref{%s}"
           lunaryorn-reftex-find-ams-environment-caption
           ("theorem" "th.") -3)
          ("example" ?x "ex:" "~\\ref{%s}"
           lunaryorn-reftex-find-ams-environment-caption
           ("example" "ex") -3)
          ;; Algorithms package
          ("algorithm" ?a "alg:" "~\\ref{%s}"
           "\\\\caption[[{]" ("algorithm" "alg") -3)))

;;Provide basic RefTeX support for biblatex
(unless (assq 'biblatex reftex-cite-format-builtin)
  (add-to-list 'reftex-cite-format-builtin
                 '(biblatex "The biblatex package"
                            ((?\C-m . "\\cite[]{%l}")
                             (?t . "\\textcite{%l}")
                             (?a . "\\autocite[]{%l}")
                             (?p . "\\parencite{%l}")
                             (?f . "\\footcite[][]{%l}")
                             (?F . "\\fullcite[]{%l}")
                             (?x . "[]{%l}")
                             (?X . "{%l}")))))


(defun latex-help-get-cmd-alist () ;corrected version:
  "Scoop up the commands in the index of the latex info manual.
   The values are saved in `latex-help-cmd-alist' for speed."
    ;; mm, does it contain any cached entries
    (if (not (assoc "\\begin" latex-help-cmd-alist))
        (save-window-excursion
  	(setq latex-help-cmd-alist nil)
  	(Info-goto-node (concat latex-help-file "Command Index"))
          (end-of-buffer)
          (while (re-search-backward "^\\* \\(.+\\): *\\(.+\\)\\." nil t)
            (setq key (ltxh-buffer-substring (match-beginning 1) (match-end 1)))
            (setq value (ltxh-buffer-substring (match-beginning 2) (match-end 2)))
            (setq latex-help-cmd-alist
                  (cons (cons key value) latex-help-cmd-alist))))
      )
    latex-help-cmd-alist
    )


(defun lew/reftex-find-ams-enviroment-cation (environment)
  "Find the caption of an AMS ENVIRONMENT."
  (let ((re (rx-to-string `(and "\\begin{" ,environment "}"))))
    ;; Go to the beginning of the label first
    (re-search-backward re)
    (goto-char (match-end 0)))
  (if (not (looking-at (rx (zero-or-more space) "[")))
      (error "Environment %s has no title" environment)
    (let ((beg (match-end 0)))
      ;; Move point onto the title start bracket and move over to the end,
      ;; skipping any other brackets in between, and eventually extract the text
      ;; between the brackets
      (goto-char (1- beg))
      (forward-list)
      (buffer-substring-no-properties beg (1- (point))))))

(defun TeX-insert-single-quote (arg)
      (interactive "p")
      (cond
       (mark-active
        (let ((skeleton-end-newline nil))
          (skeleton-insert
           `(nil ?` _ ?') -1)))
       ((or (looking-at "\\<")
            (looking-back "^\\|\\s-\\|`"))
        (insert "`"))
       (t
        (self-insert-command arg))))


(provide 'init-tex)
