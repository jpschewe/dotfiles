;;; DO NOT MODIFY THIS FILE
(if (featurep 'ede-autoloads) (error "Feature ede-autoloads already loaded"))
;;; ede-loaddefs.el --- Auto-generated CEDET autoloads

;;;### (autoloads (ede-pmake-varname) "ede-pmake" "ede/ede-pmake.el")

(autoload 'ede-pmake-varname "ede-pmake" "\
Convert OBJ into a variable name name, which converts .  to _." nil nil)

;;;***

;;;### (autoloads (ede-update-version) "ede-util" "ede/ede-util.el")

(autoload 'ede-update-version "ede-util" "\
Update the current projects main version number.
Argument NEWVERSION is the version number to use in the current project." t nil)

;;;***

;;;### (autoloads (ede-target-parent ede-parent-project ede-load-project-file ede-description ede-name project-make-dist project-compile-target project-compile-project project-edit-file-target ede-compile-target ede-remove-file ede-project ede-target) "ede" "ede/ede.el")

(autoload 'ede-target "ede" "\
A top level target to build." nil nil)

(autoload 'ede-project "ede" "\
Top level EDE project specification.
All specific project types must derive from this project." nil nil)

(defvar ede-projects nil "\
A list of all active projects currently loaded in Emacs.")

(autoload 'ede-remove-file "ede" "\
Remove the current file from targets.
Optional argument FORCE forces the file to be removed without asking." t nil)

(autoload 'ede-compile-target "ede" "\
Compile the current buffer's associated target." t nil)

(autoload 'project-edit-file-target "ede" "\
Edit the target OT associated w/ this file." nil nil)

(autoload 'project-compile-project "ede" "\
Compile the entire current project OBJ.
Argument COMMAND is the command to use when compiling." nil nil)

(autoload 'project-compile-target "ede" "\
Compile the current target OBJ.
Argument COMMAND is the command to use for compiling the target." nil nil)

(autoload 'project-make-dist "ede" "\
Build a distribution for the project based on THIS project." nil nil)

(autoload 'ede-name "ede" "\
Return the name of THIS targt." nil nil)

(autoload 'ede-description "ede" "\
Return a description suitible for the minibuffer about THIS." nil nil)

(autoload 'ede-load-project-file "ede" "\
Project file independent way to read in FILE." nil nil)

(autoload 'ede-parent-project "ede" "\
Return the project belonging to the parent directory.
nil if there is no previous directory.
Optional argument OBJ is an object to find the parent of." nil nil)

(autoload 'ede-target-parent "ede" "\
Return the project which is the parent of TARGET.
It is recommended you track the project a different way as this function
could become slow in time." nil nil)

;;;***

(add-to-list 'auto-mode-alist '("Project\\.ede" . emacs-lisp-mode))

(provide 'ede-autoloads)
