(require 'package)

(setq package-user-dir (expand-file-name "elpa" jl-emacs-root-dir))

(setq package-archives
      '(;;("gnu" . "https://elpa.gnu.org/packages/")
        ;;("melap" . "https://melpa.org/packages/")
        ;;("melpa-stable" . "https://stable.melpa.org/packages/")

        ;; Use either 163 or tsinghua mirror repository when official melpa is slow or shutdown.

        ;; 163 mirror repository
         ;; ("gnu" . "https://mirrors.163.com/elpa/gnu/")
         ;; ("melpa" . "https://mirrors.163.com/elpa/melap/")
         ;; ("melpa-stable" . "https://mirrors.163.com/elpa/melpa-stable/")

        ;; tsinghua mirror repository
        ("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")

        ))

(setq package-pinned-packages '())

;; Fire up package.el
(setq package-enable-at-startup nil)
(package-initialize)

(unless (file-exists-p (expand-file-name "elpa" jl-emacs-root-dir))
  (package-refresh-contents))

;; This functions has a macro '--each' from dash.el.
;;(defun packages-install (packages)
;;  (--each packages
;;    (when (not (package-installed-p it))
;;      (package-install it)))
;;  (delete-other-windows))

;; Install the user selected packages
(defun packages-install (packages)
  "When PACKAGES in user's list are not comtented, try to install given PACKAGES."
(dolist (pkg packages)
  (when (not (package-installed-p pkg))
    (package-install pkg))))


;;; On-demand installation of packages

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))


(provide 'setup-package)
