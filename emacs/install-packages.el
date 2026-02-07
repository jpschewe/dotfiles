(require 'package)

(package-refresh-contents)

(package-install-selected-packages)

;; prompt to update update all packages
(let ((upgrade-count (length (package-menu--find-upgrades))))
  (if (> upgrade-count 0)
      (if (yes-or-no-p (format "%d packages can be upgraded. Do it now? " upgrade-count))
          (progn
            (package-menu-mark-upgrades)
            (package-menu-execute t)
            (message "Upgrades complete."))
        (message "Upgrade cancelled."))
    (message "All packages are up to date.")))

;; compile packages
(byte-recompile-directory package-user-dir nil 'force)
