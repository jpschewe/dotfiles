;; get list of packages to install
(setq custom-file (expand-file-name "~/.xemacs/custom"))
(load custom-file)

(load (expand-file-name "~/.xemacs/package-config"))

;; include user packages in the list of packages to update
(dolist (fileOrDir (directory-files package-user-dir))
  (if (not (string= "." (substring fileOrDir 0 1)))
      (let ((path (expand-file-name fileOrDir package-user-dir)))
	(if (file-directory-p path)
	    (add-to-list 'load-path path)
	  ))
     ))

(message (format "Load path: %s" load-path))

(require 'package)

(package-refresh-contents)

;; will prompt for packages to install
(package-install-selected-packages)


;; prompt to update update all packages
(let ((upgradeable (package--upgradeable-packages)))
  (if (not upgradeable)
      (message "No packages to upgrade")
    (if (yes-or-no-p (format "Packages to upgrade: %s. Do it now? " upgradeable))
        (package-upgrade-all nil))))

;(let ((upgrade-count (length (package-menu--find-upgrades))))
;  (if (> upgrade-count 0)
;      (if (yes-or-no-p (format "%d packages can be upgraded. Do it now? " upgrade-count))
;          (progn
;            (package-menu-mark-upgrades)
;            (package-menu-execute t)
;            (message "Upgrades complete."))
;        (message "Upgrade cancelled."))
;    (message "All packages are up to date.")))

;; compile packages
(byte-recompile-directory package-user-dir nil 'force)
(byte-recompile-directory jps-lisp-path nil 'force)
