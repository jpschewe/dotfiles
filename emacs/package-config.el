(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/"))))


  (when (< emacs-major-version 28)
    (add-to-list 'package-archives
                 '("nongnu" . "https://elpa.nongnu.org/nongnu/")))
  )

;; pin packages to melpa-stable by default
(setq use-package-always-pin "melpa-stable")

(defvar jps-lisp-path (expand-file-name "~/.xemacs/xemacs-packages/lisp") "My additional lisp files that aren't available in MELPA.")
(let ((base-dir jps-lisp-path))
  (add-to-list 'load-path base-dir)
  (dolist (fileOrDir (directory-files base-dir))
    (if (not (string= "." (substring fileOrDir 0 1)))
	(let ((path (expand-file-name fileOrDir jps-lisp-path)))
	  (if (file-directory-p path)
	      (add-to-list 'load-path path)
	    )))))
