;;; DO NOT MODIFY THIS FILE
(if (featurep 'common-autoloads) (error "Feature common-autoloads already loaded"))
;;; cedet-loaddefs.el --- Auto-generated CEDET autoloads

;;;### (autoloads (cedet-update-autoloads) "cedet-autogen" "common/cedet-autogen.el")

(autoload 'cedet-update-autoloads "cedet-autogen" "\
Update autoloads in file LOADDEFS from sources.
Optional argument DIRECTORY, specifies the directory to scan for
autoloads.  It defaults to the current directory.
DIRECTORIES is a list of extra directory to scan.  Those directory
names are relative to DIRECTORY.  If DIRECTORIES is nil try to scan
sub directories of DIRECTORY where a `cedet-autogen-tagfile' file
exists." t nil)

;;;***

;;;### (autoloads (compare-strings) "cedet-compat" "common/cedet-compat.el")

(autoload 'compare-strings "cedet-compat" "\
Compare the contents of two strings.
In string STR1, skip the first START1 characters and stop at END1.
In string STR2, skip the first START2 characters and stop at END2.
END1 and END2 default to the full lengths of the respective strings.

Case is significant in this comparison if IGNORE-CASE is nil.

The value is t if the strings (or specified portions) match.
If string STR1 is less, the value is a negative number N;
  - 1 - N is the number of characters that match at the beginning.
If string STR1 is greater, the value is a positive number N;
  N - 1 is the number of characters that match at the beginning." nil nil)

;;;***

;;;### (autoloads (inversion-upgrade-package inversion-require) "inversion" "common/inversion.el")

(autoload 'inversion-require "inversion" "\
Declare that you need PACKAGE with at least VERSION.
PACKAGE might be found in FILE.  (See `require'.)
Throws an error if VERSION is incompatible with what is installed.
Optional argument DIRECTORY is a location where new versions of
this tool can be located.  If there is a versioning problem and
DIRECTORY is provided, inversion will offer to download the file.
Optional argument RESERVED is saved for later use." nil nil)

(autoload 'inversion-upgrade-package "inversion" "\
Try to upgrade PACKAGE in DIRECTORY is available." t nil)

;;;***

;;;### (autoloads (pprint-function pprint pprint-to-string) "pprint" "common/pprint.el")

(autoload 'pprint-to-string "pprint" "\
Return a string containing the pretty-printed representation of OBJECT.
OBJECT can be any Lisp object.  Quoting characters are used as needed
to make output that `read' can handle, whenever this is possible.  The
pretty printer try as much as possible to limit the length of lines to
given WIDTH.  WIDTH value defaults to `fill-column'." nil nil)

(autoload 'pprint "pprint" "\
Output the pretty-printed representation of OBJECT, any Lisp object.
Quoting characters are printed as needed to make output that `read'
can handle, whenever this is possible.  Output stream is STREAM, or
value of `standard-output' (which see).  The pretty printer try as
much as possible to limit the length of lines to given WIDTH.  WIDTH
value defaults to `fill-column'." nil nil)

(autoload 'pprint-function "pprint" "\
See a pretty-printed representation of FUNCTION-NAME." t nil)

;;;***

(add-hook 'edebug-setup-hook (lambda nil (require 'cedet-edebug) (defalias 'edebug-prin1-to-string 'cedet-edebug-prin1-to-string)))

(provide 'common-autoloads)
