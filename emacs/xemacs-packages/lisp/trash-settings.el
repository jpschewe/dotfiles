;;; trash-settings.el - Intelligent integration with system trash
;; https://stackoverflow.com/questions/2583541/problem-in-delete-directory-with-enabled-delete-by-removing-to-trash

;; Use the system trash, except for temp files and stuff
(require 'cl)

(defcustom system-trash-exclude-names
  nil
  "List of file names to exclude from system trash.
The names in this variable are matched only against the basename
of the file to be deleted."
  :type '(repeat string)
  :group 'trash)

(defcustom system-trash-exclude-paths
  nil
  "List of absolute paths to exclude from system trash.
If a path to a directory is excluded, then all the contents of that directory are also excluded."
  :type '(repeat string)
  :group 'trash)

(defcustom system-trash-exclude-matches
  nil
  "List of regexps or functions matching file names to exclude from system trash.
The matches are only applied against the file name, not the path."
  :type '(repeat (choice regexp function))
  :group 'trash)

(defcustom system-trash-exclude-path-matches
  nil
  "List of regexps or functions matching paths to exclude from system trash.
The matches are applied against the full path."
  :type '(repeat (choice regexp function))
  :group 'trash)

(defun call-process-discard-output (program &rest args)
  "Execute program with args without saving any output.
In particular, no temp files are created."
  (eval (append `(call-process ,program nil nil nil) args)))

(defun string-begins-with-p (string beginning)
  "Return t if and only if string begins with beginning"
  (string-match-p (concat "^" (regexp-quote beginning)) string))

(defun file-excluded-from-system-trash-p (path)
  "Returns non-nil if file name is excluded from trash."
  (let ((basename (file-name-nondirectory path)))
    (or
     (some (apply-partially 'string= basename)
           system-trash-exclude-names)
     (some (apply-partially 'string-begins-with-p path)
           system-trash-exclude-paths)
     (some (lambda (match)
             (funcall
              (cond ((stringp match) 'string-match-p)
                    ((functionp match) match)
                    (t 'ignore))
              match
              basename))
           system-trash-exclude-matches)
     (some (lambda (match)
             (funcall
              (cond ((stringp match) 'string-match-p)
                    ((functionp match) match)
                    (t 'ignore))
              path))
           system-trash-exclude-path-matches))))

(defadvice delete-directory (around no-recursive-trash activate)
  "When trashing a directory, there's no need to trash its contents first."
  (if delete-by-moving-to-trash
      (move-file-to-trash directory)
    ad-do-it))

(defadvice dired-delete-file (around no-recursive-trash activate)
  "When trashing a directory, there's no need to trash its contents first.
There's also no need to ask, because it's undoable."
  (if delete-by-moving-to-trash
      (move-file-to-trash file)
    ad-do-it))

(defvar jps-conditional-trash-running nil
  "Guard to prevent infinite recursion in jps-conditional-trash` advice.")

(defadvice move-file-to-trash (around jps-conditional-trash activate)
  "Conditionally move files to trash based on filename."
  (if jps-conditional-trash-running
      ad-do-it
    (let ((jps-conditional-trash-running t)
          (filename (ad-get-arg 0)))
      (if (file-excluded-from-system-trash-p filename)
          (progn
            (message "Calling delete on %s" filename)
            ;; Permanently delete if not trashing
            (if (file-directory-p filename)
                (delete-directory filename) 
              (delete-file filename)))
        ad-do-it ;; Call the original move-file-to-trash
        ))))

(provide 'trash-settings)
