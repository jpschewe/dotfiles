;;; DO NOT MODIFY THIS FILE

;;; eieio-loaddefs.el --- Auto-generated CEDET autoloads

;;;### (autoloads (call-tree) "call-tree" "eieio/call-tree.el")

(autoload 'call-tree "call-tree" "\
Build a call tree to show all functions called by FUNC." t nil)

;;;***

;;;### (autoloads (eieio-describe-generic eieio-describe-class eieio-browse) "eieio-opt" "eieio/eieio-opt.el")

(autoload 'eieio-browse "eieio-opt" "\
Create an object browser window to show all objects.
If optional ROOT-CLASS, then start with that, otherwise start with
variable `eieio-default-superclass'." t nil)

(defalias 'describe-class 'eieio-describe-class)

(autoload 'eieio-describe-class "eieio-opt" "\
Describe a CLASS defined by a string or symbol.
If CLASS is actually an object, then also display current values of that obect." t nil)

(defalias 'describe-method 'eieio-describe-generic)

(defalias 'describe-generic 'eieio-describe-generic)

(defalias 'eieio-describe-method 'eieio-describe-generic)

(autoload 'eieio-describe-generic "eieio-opt" "\
Describe the generic function GENERIC.
Also extracts information about all methods specific to this generic." t nil)

;;;***

;;;### (autoloads (enable-visual-studio-bookmarks) "linemark" "eieio/linemark.el")

(autoload 'enable-visual-studio-bookmarks "linemark" "\
Bind the viss bookmark functions to F2 related keys.
\\<global-map>
\\[viss-bookmark-toggle]     - To=ggle a bookmark on this line.
\\[viss-bookmark-next-buffer]   - Move to the next bookmark.
\\[viss-bookmark-prev-buffer]   - Move to the previous bookmark.
\\[viss-bookmark-clear-all-buffer] - Clear all bookmarks." t nil)

;;;***

;;;### (autoloads (lmcompile-do-highlight) "lmcompile" "eieio/lmcompile.el")

(autoload 'lmcompile-do-highlight "lmcompile" "\
Do compilation mode highlighting.
Works on grep, compile, or other type mode." t nil)

;;;***

;;;### (autoloads (directory-tree-thing eieio-class-tree tree-test-it-all) "tree" "eieio/tree.el")

(autoload 'tree-test-it-all "tree" "\
Try using various features of tree mode in a demo of it's display." t nil)

(autoload 'eieio-class-tree "tree" "\
Displays a class tree using the TREE package in another buffer.
Optional argument ROOT-CLASS is the starting point." t nil)

(autoload 'directory-tree-thing "tree" "\
Start at the current directory, and build a giant tree of files.
Argument PPATH is the path to the directory we are going to analyze." t nil)

;;;***


