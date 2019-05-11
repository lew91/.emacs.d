
;;; Customize group
;;; Code:

(defgroup jakelew nil
  "My Emacs customizations."
  :group 'convenience)

(defcustom jakelew-logo nil ;(expand-file-name "logo.png" user-emacs-directory)
  "Set logo nil means official logo."
  :type 'string)

(defcustom jakelew-full-name ""
  "Set user full name."
  :type 'string)

(defcustom jakelew-mail-address ""
  "Set user email address."
  :type 'string)

(defcustom jakelew-proxy "127.0.0.1:1087"
  "Set network proxy."
  :type 'string)

(defcustom jakelew-package-archives 'melpa
  "Set package archives from which to fetch."
  :type '(choice
          (const :tag "Melpa" melpa)
          (const :tag "Melpa Mirror" melpa-mirror)
          (const :tag "Emacs-China" emacs-china)
          (const :tag "Netease" netease)
          (const :tag "Tuna" tuna)))

(provide 'jakelew-config)
;; jakelew-config.el ends here


