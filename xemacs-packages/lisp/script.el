;;; script.el --- script mode

;; Copyright (C) 1995 Richard Sharman

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; A major mode for editing 3 types of scripts:
;; Bourne Shell,  C-shell and AWK.
;;
;; The command script-mode isn't usually invoked directly.
;; Instead,  invoke one of the derived modes:
;;   script-sh-mode,  script-bash-mode, script-csh-mode, script-awk-mode.
;; This can be done automatically using auto-mode-alist and
;; interpreter-mode-alist.  
;; (For zsh, use script-bash-mode, but not all cases are handled.)
;; To customize indentation (which is all this package provides!),
;; see User-settable preferences.

;; Example of using this in conjunction with interpreter-mode-alist, 
;; which is called when a file is visited that doesn't have an entry
;; in auto-mode-alist and whose first line contains "#!":
;;
;;(setq interpreter-mode-alist
;;      (append
;;      ;; Note; these are strings, not regexps.
;;      ;; Use script-bash-mode not script-sh-mode for the first one
;;      ;;   if your shell is actually bash.
;;      '(( "sh" . script-sh-mode )  
;;       ( "bash" . script-bash-mode )
;;       ( "awk" . script-awk-mode )
;;       ( "gawk" . script-awk-mode )
;;       ( "nawk" . script-awk-mode )
;;       ( "tcsh" . script-csh-mode )
;;       ( "csh" . script-csh-mode ))
;;       interpreter-mode-alist))

;; Example of setting mode depending on file name:
;;
;; (setq auto-mode-alist
;;       (append '(
;;            ("\\.awk$"  . script-awk-mode) ;; (replaces std awk-mode)
;;                 )
;;         auto-mode-alist))

;; Hooks:
;; script-mode-hook is common to all modes;  additionally there are
;; 4 specific script-XXX-mode-hook's which are run after script-mode-hook.

;; Example of a hook, specific to bash mode, to have comments
;; starting in first column:
;;
;;(defun my-script-bash-mode-hook ()
;;  (setq script-tables (append
;;     ;; This precedes existing script-tables, thus it is considered first.
;;              '(
;;                (this "#" script-first-col)
;;                )
;;              script-tables))
;;  )
;; (setq script-bash-mode-hook 'my-script-bash-mode-hook)

;; Autoloads:
;; ----------
;; (autoload 'script-sh-mode "script" "editing mode for sh scripts" t)
;; (autoload 'script-bash-mode "script" "editing mode for bash scripts" t)
;; (autoload 'script-csh-mode "script" "editing mode for csh scripts" t)
;; (autoload 'script-awk-mode "script" "editing mode for awk scripts" t)

;; Feature:
;; -------
;; If successfully loaded, it provides feature script.


;; Restrictions
;; ------------
;; - sh/bash-modes:  A closing keyword (fi or done) is not detected
;;              if it is not the first symbol on the line.  This fails:
;;                      for a in 1 2 3 4 ; do
;;                          echo $a ; done
;;                          this is wrongly indented
;;              Special cases for one-liner if/for are supported.
;;
;; - csh-mode:  Not all valid continuation forms are handled, e.g.
;;              a continuation inside the (...) of an if/foreach/while.
;; - awk-mode:  Multiple statement separated by a ';' where the first
;;              statement is the body of an if/else/for/while.
;;              e.g.   if (aaa)
;;                         bbb ; ccc
;;                         this_indendation_is_wrong


;;; User-settable preferences:

(defvar script-auto-indent nil
  "*Non-nil means a Return automatically reindents the line,
does the newline, and then reindents the new empty line.
By default, Linefeed does this always.")

(defvar script-default-indent 4
  "*The value used to indent relative to the previous line when
none of the more specific cases apply.
See also script-case-label-offset, script-case-body-offset,
script-offset-for-contin, script-offset-of-do and
script-offset-after-do."
  )

(defvar script-case-label-offset 4
  "*Used for the indentation of a case label relative to the
case statement itself (or break statement in csh).")

(defvar script-case-body-offset 6
  "*Used for the indentation of a case body relative to the
case statement itself (or break statement in csh).")

(defvar script-offset-for-contin 4
  "*Used for the indentation for continued lines, as determined by
script-continued-regexp.")

;; By switching which of the next two is zero and non-zero, you
;; can change the appearance of a standalone "do" or "then" line:
;;
(defvar script-offset-of-do 2
  "*This controls the offset of the do or then line relative to a
preceding for, while or if line.
See also script-offset-after-do.")
;;
(defvar script-offset-after-do 0
  "*This controls the offset of lines following do or then.
See also script-offset-of-do.")

(defvar script-comments-var
  ;; This variable is used by function  script-handle-comment.
  t
  "*How comments are handled.  Can be set to:
* a constant, e.g. 0 to make in first column,
* t to leave unchanged,   or
* the name of a function e.g. 'my-comment-handler which is called
  and which should return the indentation column.

Use \(setq script-comments-var 'script-indent-to-same)
to use the previous line's indentation.
")


(defvar script-debug nil
  "*Set non-nil to debug script-check-indent-table.")

;;; Theory
;;; ======
;;; Indendation is controlled by calling script-check-indent-table
;;; with a table which is mode-specific.  This table is a list of
;;; items, each of which is a list of 3 or 4 fields.
;;;   Field 1 - one of 3 symbols:  prev, this, prevs.  This denotes
;;;             what entity is to be checked:  the previous line [or
;;;             stmt], this line,  or the simple previous line (excluding
;;;             continuations).
;;;   Field 2 - a regexp.  
;;;   Field 3 - a function
;;;   Field 4 - an optional argument, typically a number.
;;; If the entity denoted by field1  matches the regexp in field2,
;;; then function field3 is called [at that entity] passing it field4
;;; if present. 
;;; If the function returns
;;;   nil - there is no match, and the next item is tried.
;;;   t   - the line is to be left as is
;;;   a number - this is the new indentation of the line.
;;; This is done until a non-nil value is returned, or the end of the
;;; table is found.


;;; general functions

(defun script-msg-wait (msg)
  (message msg)
  (sit-for 1))

(defun script-newline (&optional arg)
  "If script-auto-indent is nil, behaves like standard newline.
If script-auto-indent is non-nil, then it first indents the current
line before inserting new line,  and indents the new empty line after."
  (interactive "*P")
  (if script-auto-indent
      (reindent-then-newline-and-indent)
    (newline arg)))

(defun script-find-nearest (a b &optional limit)
  (if (re-search-backward 
       (concat "\\(" a "\\)\\|\\(" b "\\)") limit t)
      (if (match-beginning 1)
          -1
        1)
    nil))

(defun script-find-prev-matched (open close &optional limit)
  (let ( (count 1) (found nil) (going t) n )
    (while (and going (not (zerop count)))
      (if (setq n (script-find-nearest open close limit))
          (setq count (+ count n))
        (setq going nil)))
    (if (zerop count)
        (point)
      nil)
    ))

(defun script-keep-as-is ()
  "A function returning t means keep the line unchanged."
  t)

;; The following isn't actually used here, except as an example
;; of use in a hook.
(defun script-first-col ()
  "A function returning a number means indent to a specific column
number, starting from zero."
  0)



(defun script-indent-right ()
  "Indent right by the default amount"
  ;; prev-indent may be nil at beginning of buffer.
  (if prev-indent
      (+ prev-indent script-default-indent)))

(defun script-indent-right-n (&optional args)
  "Indent right by a specific amount."
  ;; Should  script-indent-right  be removed,  or should we
  ;; remove the optionality of the args?  It's silly as it is.
  (if (null args)
      (+ prev-indent script-default-indent)
    (+ prev-indent (eval (car args)))))



(defun script-indent-left ()
  "Indent left by the default amount."
  (- prev-indent script-default-indent))

(defun script-prev-simple-line ()
  (interactive)
  (let ( (n 1) (start (point))  )
    (beginning-of-line)
    (while (and (zerop (setq n (forward-line -1)))
                (or
                 (looking-at "\\s-*$")
                 (looking-at script-comment)
                 ))
      )
    (if (zerop n)
        (progn
          (skip-chars-forward " \t")
          (point));; success
      (goto-char start)
      nil)
    ))

(defun script-find-prev-line ()
  (interactive)
  (let ( (found nil) )
    (if (script-prev-simple-line)
        (progn
          (setq found (point))
          (if (and (script-prev-simple-line) 
                   (looking-at script-continued-regexp))
              (while (and
                      (looking-at script-continued-regexp)
                      (setq found (point))
                      (script-prev-simple-line))))
          (goto-char found))
      nil
      )))

(defun script-prev-is-contin ()
  ;; If previous non-blank non-comment line is a continuation
  ;; return its location, else return nil.
  (interactive)
  (let ( (n 1) (start (point)) )
    (save-excursion
      (beginning-of-line)
      (while (and (zerop (setq n (forward-line -1)))
                  (or
                   (looking-at "\\s-*$")
                   (looking-at script-comment))
                  ))
      (if (and (zerop n) (looking-at script-continued-regexp))
          (point)
        nil))
    ))



(defun script-indent-to-same ()
  "Indent this line the same as the previous line.
Except,  if that line was indented because it was a continuation of
a previous line, then align with its start."
  (interactive)
  (let ( (val (current-indentation)) )
    (if prev
        (progn
          (goto-char prev)
          (setq val (current-indentation))
          (if (script-prev-is-contin)
              (progn
                (script-find-prev-line);; find start of this contin line
                (setq val (current-indentation)))
            )))
    val
    ))

(defun script-check-indent-table (table)
  (let ( (p table)
         (found nil)
         (prev-indent nil)
         (prev nil)
         (prevs nil)
         temp)
    (save-excursion
      (setq prevs (script-prev-simple-line)))
    (save-excursion
      (setq prev (script-find-prev-line))
      (setq prev-indent (current-indentation)))
    (beginning-of-line)
    (skip-chars-forward " \t")
    (while (and p (null found))
      (setq item (car p))
      (if (and script-debug (numberp script-debug))
          (script-msg-wait (format "at %s %s"
				   (buffer-substring (point) (+ (point) 8))
				   (prin1-to-string item))))
      (cond
       ((eq (nth 0 item) 'this)
        (save-excursion
          ;; The eval below is needed if use a variable in (nth 1 item)
          (if (looking-at (eval (nth 1 item)))
              (if (nth 3 item)
                  (setq found (funcall (nth 2 item) (nthcdr 3 item) ))
                (setq found (funcall (nth 2 item) )))
            )))
       ((eq (nth 0 item) 'prev);; prev line / prev stmt
        (if prev
            (save-excursion
              (save-excursion
                (goto-char prev)
                (setq temp (looking-at (eval (nth 1 item)))))
              (if temp
                  (if (nth 3 item)
                      (setq found (funcall (nth 2 item) (nthcdr 3 item)))
                    (setq found (funcall (nth 2 item)))))
              )))
       ((eq (nth 0 item) 'prevs);; prev simple line
        (if prevs
            (save-excursion
              (save-excursion
                (goto-char prevs)
                (setq temp (looking-at (eval (nth 1 item)))))
              (if temp
                  (if (nth 3 item)
                      (setq found (funcall (nth 2 item) (nthcdr 3 item)))
                    (setq found (funcall (nth 2 item)))))
              )))

       ((eq (nth 0 item) t)
        (if (nth 3 item)
            (setq found (funcall (nth 2 item) (nthcdr 3 item)))
          (setq found (funcall (nth 2 item)))))
       (t
        (error (format
                "script-check-indent-table: bad item found %s"
                (prin1-to-string (nth 0 item))
                ))))
      (if script-debug
          (if found
              (script-msg-wait (format "match on %s" (prin1-to-string item)))))
      (setq p (cdr p)))
    found
    ))


(defun script-calculate-indent ()
  "Return appropriate indentation for current line as script code.
In usual case returns an integer: the column to indent to.
Returns nil if unkonwn,  returns t if no change to indentation."
  (save-excursion
    (beginning-of-line)
    ;; Note: user may have case-fold-search t, but we want our
    ;; searches to be case sensitive.
    (let (  (case-fold-search nil)  )
      (script-check-indent-table script-tables))))



(defun script-indent-line ()
  "Indent current line as script."
  (let* ((indent (script-calculate-indent))
         (pos (- (point-max) (point))))
    (beginning-of-line)
    (cond ((eq indent nil)
           nil )
          ((eq indent t)
           nil )
          ((numberp indent)
           (setq left-margin indent)
           (indent-to-left-margin))
          (t
           (error "script-indent-line - unknown indent")))
    ;; If initial point was within line's indentation,
    ;; position after the indentation.  Else stay at same point in text.
    (if (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos)))
    ))

;;; regexps

;; this is common to all script-xxx-modes
(setq script-comment "\\s-*#")


;;;             specific stuff


;;;             sh specific

;; sh regexps

;; script-contin is used to build up exprssions
(setq script-contin    "\\(.*\\\\\n\\)*")

(setq script-sh-do-regexp
      (concat
       "\\<\\(for\\|while\\|until\\)\\>"
       script-contin
       ".*[\n;]\\s-*do")
      )


(setq script-sh-if-regexp
      (concat 
       "\\<if\\>"
       script-contin
       ".*[\n;]\\s-*then\\>"
       ))


(setq script-sh-case-regexp
      "\\<case\\>.*\\<in\\>" )

;; This is not foolproof, e.g. fails on nested parens.
(setq script-possible-extra-right-paren-regexp 
      "\\([^()\n]*[(][^)\n]*[)]\\)*[^()\n]*)"
      )

;; This is used to set script-continued-regexp in all modes
;; except awk.
(setq script-default-continued-regexp ".*\\\\$")

(defun script-indent-for-continued ()
  (+ prev-indent script-offset-for-contin))

(setq script-basic-table
      '(
        ;; Originally there was more common stuff here, such as 
        ;; continuation and '}' handling.  However, changes in awk
        ;; handling gradually moved stuff out.
        ;; (prevs       script-continued-regexp  script-indent-for-continued)
        ;; So, all that's left now is the default case!
        ;; Default - if there's a previous line, use that indent
        (t nil script-indent-to-same)
        ))


(setq script-sh-table
      '(
        (this comment-start script-handle-comment )
        ;; Check continuation first
        (prevs  script-continued-regexp  script-indent-for-continued)
        ;;
        (this   "\\<done\\>"    script-sh-indent-to-do)
        ;; if ... fi
        (this   "\\<fi\\>"      script-sh-indent-to-if)
        ;; this fails on one-line if:
        ;; (prev        ".*;\\s-*then\\>"       script-indent-right)
        (prev   ".*;\\s-*then\\>"       script-check-no-fi)
        (this   "then\\>"       script-indent-right-n script-offset-of-do)
        (prev   "then\\>"       script-indent-right-n script-offset-after-do)
        ;;
        (this   "\\(else\\|elif\\)\\>"  script-sh-indent-to-if)
        (prev   "\\(else\\|elif\\)\\>"  script-indent-right)
        ;;
        ;; while/for ... done
        (prev   ".*;\\s-*do\\>" script-check-no-done)
        ;;
        (this   "do\\>"         script-indent-right-n script-offset-of-do)
        (prev   "\\s-*do\\>"    script-indent-right-n script-offset-after-do)
        ;;
        ;; case stuff
        (prev   "case.*in\\s-*$"        script-indent-for-sh-case-label)
        ;; this entry must come before the one checking for ";;"
        (this   "\\<esac\\>"            script-sh-indent-of-case)
        ;; 
        ;; Check end of prev non-blank line,  not prev stmt, since
        ;; otherwise it fails if stmt is continued.
        (prevs  ".*;;\\s-*$"            script-indent-for-sh-case-label)
        (prevs  ".*)\\s-*$"             script-check-new-case-body)
        ;;
        (prev script-possible-extra-right-paren-regexp
              script-check-new-case-body)
        ;; { and } for functions
        ;; ( and ) also for groups
        (prev ".*[{(]\\s-*$" script-indent-right)
        (this "[})]" script-indent-left)
        ))


(defun script-call-or-eval (x)
  (if (symbolp x)
      (progn
        (cond 
         ((fboundp x)
	  (funcall x))
         (t
          (eval x))))
    x))

(defun script-handle-comment ()
  "Handling of comments. See documentation for script-comments-var."
  (script-call-or-eval script-comments-var))


(defun script-indent-for-sh-case-label ()
  (+ (script-sh-indent-of-case) script-case-label-offset))

(defun script-indent-for-sh-case-body ()
  (let ( (temp (script-sh-indent-of-case)) )
    (if temp
        (+ (script-sh-indent-of-case) script-case-body-offset)
      nil)
    ))

(defun script-check-new-case-body ()
  "This is called when previous seems to be a case label.
However, false matches can occur, e.g. on lines like
\(cd abc ; def )
and because script-possible-extra-right-paren-regexp doesn't handle
nested parentheses.
So we check the line before that to see if it matches a case
or an end of case body."
  (let ( (ret nil) )
    (save-excursion
      (setq ret
            (and (script-find-prev-line);; possible extra ')'
                 (script-find-prev-line);; is it ";;" or case ?
                 (or (looking-at (concat "\\s-*" script-sh-case-regexp))
                     (looking-at ".*;;\\s-*"))))
      )
    (if ret
        (setq ret (script-indent-for-sh-case-body)))
    ret
    ))

(defun script-sh-indent-to-do ()
  (if (script-find-prev-matched script-do-regexp 
                                "\\(^\\|.*;\\)\\s-*done\\>"
                                )
      (progn
        (if (looking-at "^\\s-*do") 
            (if (script-find-prev-line)
                (current-indentation)
              (script-msg-wait "missing thing before do???")
              nil)
          (current-indentation)
          ))
    (message "can't find matching statement for done")
    nil))

(defun script-sh-indent-to-if ()
  (if (script-find-prev-matched script-sh-if-regexp
                                ;; "\\<fi\\>"
                                "\\(^\\|.*;\\)\\s-*fi\\>")
      (current-indentation)
    (message "can't find matching statement for fi")
    nil))


;; (defun script-sh-find-matching-case ()
;;  (script-find-prev-matched "\\<case\\>" "\\<esac\\>"))

(defun script-sh-indent-of-case ()
  (save-excursion
    (if (script-find-prev-matched
         ;; "\\<case\\>" "\\<esac\\>")
         "\\(^\\|.*;\\)\\s-*case\\>"
         "\\(^\\|.*;\\)\\s-*esac\\>")
        (current-indentation)
      (message "can't find matching statement for case")
      nil)))

(defun script-check-no-fi ()
  ;; This is called when prev line contains
  ;; ... ; then
  ;; Normally we indent right.  However, if the line also contains
  ;; a "fi" this will erroneously indent this line.
  (save-excursion 
    (goto-char prev)
    (if (looking-at ".*\\<fi\\>")
	nil
      (script-indent-right))))
  
(defun script-check-no-done ()
  ;; This is called when prev line contains
  ;; ... ; do
  ;; Normally we indent right.  However, if the line also contains
  ;; a "done" this will erroneously indent this line.
  (save-excursion 
    (goto-char prev)
    (if (looking-at ".*\\<done\\>")
	nil
      (script-indent-right))))

;;; mode stuff

(defvar script-mode-syntax-table nil
  "Syntax table used while in script mode.")

(defvar script-mode-abbrev-table nil
  "Abbrev table used while in script mode.")
(define-abbrev-table 'script-mode-abbrev-table ())

(if script-mode-syntax-table
    ()
  ;; this was the old way:
  (setq script-mode-syntax-table (make-syntax-table))
  (setq script-mode-syntax-table (standard-syntax-table))
  ;; this is the new way
  ;;  (setq script-mode-syntax-table (copy-syntax-table (standard-syntax-table)))
  ;;  treat ` like quotation marks:
  (modify-syntax-entry ?` "\"" script-mode-syntax-table)
  ;; (modify-syntax-entry ?\" ".   " script-mode-syntax-table)
  ;; (modify-syntax-entry ?\\ ".   " script-mode-syntax-table)
  ;; (modify-syntax-entry ?' "w   " script-mode-syntax-table)
  ;; These are necessary to ignore comments in backward-sexp.
  (modify-syntax-entry ?\# "<   " script-mode-syntax-table)
  (modify-syntax-entry ?\n ">   " script-mode-syntax-table)
  (modify-syntax-entry ?\f ">   " script-mode-syntax-table)
  )


(defvar script-mode-map nil
  "Keymap for Script mode..")

(if script-mode-map
    ()
  (setq script-mode-map (make-sparse-keymap))
  (define-key script-mode-map "\n" 'reindent-then-newline-and-indent)
  (define-key script-mode-map "\C-M" 'script-newline) 
  )

(defun script-shell-type ()
  "See if this is a #! script; if so, return the basename.
Otherwise return nil."
  (let ( (str nil) )
    (save-excursion
      (beginning-of-buffer)
      (if (< (point) (point-max))
          (save-restriction
            (narrow-to-region (point)
                              (progn (end-of-line) (point)))
            (beginning-of-line)
            
            (if (looking-at "#![ \t]*\\([^ \t\n]*/\\)*\\([^ \t\n/]+\\)\\([ \t]\\|$\\)")
                (setq str 
                      (buffer-substring (match-beginning 2)
                                        (match-end 2)))))))
    str))

(defun script-mode ()
  "Major mode for editing shell scripts.
Special commands: \\{script-mode-map}
Turning on Script mode calls the value of the variable `script-mode-hook',
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map script-mode-map)
  (setq mode-name "Script")
  (setq major-mode 'script-mode)
  (setq local-abbrev-table script-mode-abbrev-table)
  (set-syntax-table script-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'script-indent-line)
  (make-local-variable 'script-do-regexp);; diff for sh/ksh/bash
  (make-local-variable 'script-tables)
  (make-local-variable 'script-continued-regexp)
  (setq script-continued-regexp script-default-continued-regexp)
  (make-local-variable 'comment-start)
  (setq comment-start "#")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "#+ *")
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'comment-column)
  (setq comment-column 0)		; ?
  (make-local-variable 'script-default-indent)
  (make-local-variable 'script-comments-var)
  (make-local-variable 'script-auto-indent)
  (setq script-tables (append 
		       script-basic-table
		       ))
  (run-hooks 'script-mode-hook)
  )

(define-derived-mode script-sh-mode
  script-mode "script sh"
  "Major mode for sh (Bourne shell) scripts.
\\{script-sh-mode-map}
Turning on this mode calls the value of the variables `script-mode-hook'
and `script-sh-mode-hook' if these values are non-nil.

Variables controlling indentation style:
  script-default-indent
    This is the default value of indentation,  used when none of
    the following are applicable.

  script-case-label-offset
     This is the indentation value used to indent a case label relative
     to the `case' statement itself.

  script-case-body-offset
     This is the indentation value used to indent the body of a case
     alternative, relative to the `case' statement itself.

  script-offset-for-contin
     This controls the indentation of a continuation line,  as 
     determined by script-continued-regexp.
     
  script-offset-of-do
  script-offset-after-do
     These determine indentation styles used when a `do' or `then'
     appears on a line by itself.  Usually one but not both are zero.
     Variable script-offset-of-do controls the offset of the `do' (or
     `then') relative to the preceding line (`while', `for', etc.).
     Variable script-offset-after-do controls the offsets of lines
     after the `do' (or `then') before the `done' (or `elif' or...).

  script-comments-var
     This controls the indentation of comments.  For details, 
     type \\[describe-variable] script-comments-var.

  script-auto-indent
     If non nil,  a newline reindents current line and indents
     for the new empty line.
"
  (setq script-tables (append 
                       ;; sh specific tables
                       script-sh-table
                       script-basic-table
                       ))
  (setq mode-name "sh Script")
  (setq script-do-regexp script-sh-do-regexp)
  (run-hooks 'script-sh-mode-hook)
  )


;;; ============================= bash ==================================
;;      as sh, but add:
;;      select NAME [in WORDS ...]; do COMMANDS; done

(setq script-bash-do-regexp
      "\\<\\(for\\|while\\|until\\|select\\)\\>\\(.*\\\\\n\\)*.*[\n;]\\s-*do"
      )

;; If bash needs any other stuff,  put them here:
(setq script-bash-extra-table
      '(
        ;; ???
        ))


(define-derived-mode script-bash-mode
  script-mode "script bash"
  "Major mode for bash scripts.
\\{script-bash-mode-map}
Turning on this mode calls the value of the variables `script-mode-hook'
and `script-bash-mode-hook' if these values are non-nil.

Variables controlling indentation style:
  script-default-indent
    This is the default value of indentation,  used when none of
    the following are applicable.

  script-case-label-offset
     This is the indentation value used to indent a case label relative
     to the `case' statement itself.

  script-case-body-offset
     This is the indentation value used to indent the body of a case
     alternative, relative to the `case' statement itself.

  script-offset-for-contin
     This controls the indentation of a continuation line,  as 
     determined by script-continued-regexp.
     
  script-offset-of-do
  script-offset-after-do
     These determine indentation styles used when a `do' or `then'
     appears on a line by itself.  Usually one but not both are zero.
     Variable script-offset-of-do controls the offset of the `do' (or
     `then') relative to the preceding line (`while', `for', etc.).
     Variable script-offset-after-do controls the offsets of lines
     after the `do' (or `then') before the `done' (or `elif' or...).

  script-comments-var
     This controls the indentation of comments.  For details, 
     type \\[describe-variable] script-comments-var.

  script-auto-indent
     If non nil,  a newline reindents current line and indents
     for the new empty line.
"

  (setq script-do-regexp script-bash-do-regexp)
  (setq script-tables (append
                       ;; bash specific tables
                       ;; ? script-bash-extra-table
                       script-sh-table
                       script-basic-table
                       ))
  (setq mode-name "bash Script")
  (run-hooks 'script-bash-mode-hook)
  )



;;; ============================= csh ==================================


(setq script-csh-table
      '(
        (this comment-start script-handle-comment )
        (prevs  script-continued-regexp  script-indent-for-continued)
        (this   "\\<endsw\\>"   script-csh-indent-of-case)
        (this   "\\<case\\>.*:" script-indent-for-csh-case-label)
        (prev   "\\s-*\\<case\\>.*:"    script-indent-for-csh-case-body)
        (prev   script-csh-looping-regexp       script-indent-right)
        (this   "endif\\>"      script-csh-indent-to-if)
        (this   "else\\>"       script-csh-indent-to-if)
        (prev   "else\\>"
                script-indent-right)
        (prev    script-csh-if-regexp   script-indent-right)
        ;; this should be while or foreach ???
        (this   "end\\>"        script-csh-indent-to-looping)
        ))

(defun script-csh-indent-of-case ()
  (save-excursion
    (if (script-find-prev-matched "\\<switch\\>" "\\<endsw\\>")
        (current-indentation)
      nil)))

(defun script-indent-for-csh-case-label ()
  (+ (script-csh-indent-of-case) script-case-label-offset))

(defun script-indent-for-csh-case-body ()
  (let ( (temp (script-csh-indent-of-case)) )
    (if temp
        (+ (script-csh-indent-of-case) script-case-body-offset)
      nil)
    ))



(setq script-csh-looping-regexp "\\<\\(foreach\\|while\\)\\>.*(.*)\\s-*$")

(setq script-csh-if-regexp "\\<if\\>.*\\<then\\>")


(defun script-csh-indent-to-if ()
  (if (script-find-prev-matched 
       script-csh-if-regexp
       "\\<endif\\>")
      (current-indentation)
    nil))

(defun script-csh-indent-to-looping ()
  (if (script-find-prev-matched 
       script-csh-looping-regexp
       "\\<end\\>")
      (current-indentation)
    ;; Bug: this message gets printed with point set where
    ;; script-find-prev-matched failed.  Maybe should do the above
    ;; in a save-excursion.
    (script-msg-wait "Can't find match for this end")
    nil))


(define-derived-mode script-csh-mode
  script-mode "script csh"
  "Major mode for csh/tcsh scripts.
\\{script-csh-mode-map}
Turning on this mode calls the value of the variables `script-mode-hook'
and `script-csh-mode-hook' if these values are non-nil.

Variables controlling indentation style:
  script-default-indent
    This is the default value of indentation,  used when none of
    the following are applicable.

  script-case-label-offset
     This is the indentation value used to indent a case label relative
     to the `switch' statement itself.

  script-case-body-offset
     This is the indentation value used to indent the body of a case
     alternative, relative to the `switch' statement itself.

  script-offset-for-contin
     This controls the indentation of a continuation line,  as 
     determined by script-continued-regexp.

  script-auto-indent
     If non nil,  a newline reindents current line and indents
     for the new empty line.
"

  (setq script-tables (append
                       ;; csh specific tables
                       script-csh-table
                       script-basic-table
		       ))
  ;; (setq case-fold-search nil)
  (setq mode-name "csh Script")
  (run-hooks 'script-csh-mode-hook)  
  )


;; ===================== awk ==================================

(setq script-awk-continued-regexp 
      ;; do not include "{";  this is handled separately
      ".*\\(,\\|?\\|:\\|||\\|&&\\|\\\\\\|do\\)\\s-*\n")

(setq script-awk-table
      '(
        (this comment-start script-handle-comment )
        ;;
        ;; next was prev, changed to prevs
        (prevs  script-continued-regexp script-awk-indent-continued-line)
        ;;
        (prev ".*{[^}]*$" script-indent-right)
        ;;
        ;; Next line changes value of prev; since the "previous statement" 
        ;; in awk is not as simple as it is most script modes.
        ;; It calls script-awk-prev-stmt rather than just using the 
        ;; previous non-blank line.
        (t nil script-awk-reset-prev)
        ;;
        (this   "else\\>"       script-awk-handle-else)
        ;;
        (this "}" script-indent-left-from-sexp);; was: script-indent-left)
        ;;
        (prev  "\\s-*\\(if\\|while\\|for\\)\\\>" script-awk-handle-if)
        ;;
        (prev "else\\>" script-awk-indent-else)
        ;;
        (t nil script-indent-to-same)
        ))

(defun script-awk-this-is-a-comment-line ()
  (save-excursion
    (beginning-of-line)
    (looking-at "\\s-*#")))


(defun script-awk-this-is-a-contin-line ()
  (save-excursion
    (beginning-of-line)
    (looking-at script-awk-continued-regexp)))

(defun script-awk-indent-continued-line ()
  ;; This indents relative to the previous statement (not line),
  ;; but I think this is wrong now:
  ;;  (if (script-awk-prev-stmt)
  ;;      (+ (current-indentation) script-offset-for-contin)
  ;;    t)
  (if prevs
      (save-excursion
	(goto-char prevs)
	(+ (current-indentation) script-offset-for-contin))))



(defun script-awk-reset-prev ()
  ;; set prev to previous statement, not prev line
  (save-excursion
    (setq prev (script-awk-prev-stmt))
    (setq prev-indent (current-indentation))
    nil))

(defun script-indent-left-from-sexp ()
  (forward-char 1)
  (backward-sexp)
  (current-indentation))

(defun script-awk-find-match-for-else ()
  ;; this statement is an else
  (let ( (count 1) last )
    ;; We are at an else stmt.
    (while (and (/= count 0)
                (script-awk-prev-stmt)
                (setq last (point))
                (cond
                 ((looking-at "\\(for\\|while\\)\\>")
                  t)
                 ((looking-at "if\\\>")
                  (setq count (1- count)))
                 ((looking-at "else\\\>")
                  (setq count (1+ count)))
                 (t 
                  nil))
                )
      );; no body for while
    (if (/= count 0)
	(progn
	  ;; not sure if this should print a message and return t,
	  ;; or signal an error.
	  (error "Cannot find matching if for an else")
	  ;; (script-msg-wait "Cannot find matching if for this else")
	  t)
      (goto-char last)
      )
    ))

(defun script-awk-handle-else ()
  ;; called when "this" stmt is an else
  (script-awk-find-match-for-else)
  (current-indentation))

(defun script-awk-handle-if ()
  ;; prev is if/while/for    this is not an else
  (let ( (last nil) )
    ;; Is prev a real if stmt, of just the if part of it?
    (if (script-awk-in-block-exp)
        ;; Just the if part of it, so indent after an if.
        (script-indent-right)
      ;; Prev was a complete if statement;  see if it is actually
      ;; part of a bigger if/for/while one.
      (if (script-awk-prev-stmt)
          ;; See if either this has a containing stmt,  or
          ;; is at else (in which case go to matching and continue)
          (while (or
                  (script-awk-find-containing-stmt)
                  (and (looking-at "else\\>")
                       (script-awk-find-match-for-else)))
            (setq last (point))))
      (if last
          (progn
            (goto-char last);; back to last if/for/while
            (current-indentation)))
      )))

(defun script-awk-indent-else ()
  ;; Previous seems to be else,  but actually the else could be on 
  ;; a separate line so this is really the else.
  (let ( (save (point)) result ) 
    (skip-chars-backward " \t")
    (if (and 
         (script-awk-backup-over-nl t)
         (eq (script-awk-backup) ?e)
         (looking-at "else\\>"))
        ;; actually "this" stmt is an else
        (+ (script-awk-handle-else) script-default-indent)
      ;; Find the matching if for this else,
      ;; then align with previous stmt
      (script-awk-prev-stmt);; go to the else
      (script-awk-find-match-for-else)
      ;; We are now sitting at the matching if stmt.
      ;; Normally we indent to this line,  but if this was actually
      ;; part of a simple if/for/while body itself, indent left.
      (setq result (current-indentation))
      (while (or
              (script-awk-find-containing-stmt)
              (and (looking-at "else\\>")
                   (script-awk-find-match-for-else)))
        (setq result (current-indentation)))
      result
      )))

(defun script-awk-indent-to-prev-stmt ()
  ;; Replace (current-indentation) with (current-column) to indent to
  ;; previous statement.    Usually, this looks odd though.
  (if (script-awk-prev-stmt) 
      (current-indentation)))


(defun script-awk-backup ()
  (let ( prev c)
    (skip-chars-backward " \t")
    (setq prev (point))
    (skip-chars-backward "^ \t\n;(){}[]'\"") 
    (setq c (preceding-char))
    (cond
     ((not (eq prev (point)))
      (setq c (following-char)))
     ;; did NOT move
     ((bobp)
      (setq c nil))
     ((or (eq (char-syntax c) ?\)) (eq (char-syntax c) ?\"))
      (backward-sexp 1))
     (t
      (backward-char 1)
      (setq c (following-char)))
     );; cond
    c
    ))


       
(defun script-awk-backup-over-nl (accept-noncontin-line)
  ;; If preceding char is a newline,  then try to backup over it.
  ;; Returns non-nil if there is a previous non-comment and
  ;; (optionally) non continuation line.
  (if (eq (preceding-char) ?\n)
      (let ( (save (point)) (ret nil) (going t) )
	(while (and going (null ret) )
	  (cond
	   ((/= (forward-line -1) 0)
	    ;; can't go back any more
	    (goto-char save)
	    (setq ret nil)
	    (setq going nil)
	    )
	   ;; could go back...
	   ((looking-at "\\s-*\n");; skip empty line
	    ;; continue
	    )
	   ((script-awk-this-is-a-comment-line)
	    ;; continue
	    )
	   ((script-awk-this-is-a-contin-line)
	    (end-of-line)
	    (setq ret (point))
	    )
	   ;; not comment line, not contin line
	   ;; can we accept it?
	   (accept-noncontin-line
	    (end-of-line)
	    (setq ret (point))
	    )
	   ;; no, only accept contin lines, so revert to saved
	   (t
	    (goto-char save)
	    (setq ret nil)
	    (setq going nil)
	    ))
	  );; while
	ret
	)
    ;; preceding was NOT \n
    nil
    )
  )
        


(defun script-awk-at-block-kw (&optional accept-else)
  ;; use [ \t] not \\s- so don't match a \n
  (if accept-else
      (looking-at "[ \t]*\\(if\\|while\\|for\\|else\\)\\\>")
    (looking-at "[ \t]*\\(if\\|while\\|for\\)\\\>"))
  )

(defun script-awk-prev-stmt-1  ()
  (let ( c cs (going t) prev (first-time t) (start (point)) )
    (setq prev (point))
    (while (and going (setq c (script-awk-backup)))
      (cond
       ((eq (setq cs (char-syntax c)) ?w)
        (if (looking-at "\\(if\\|while\\|else\\|for\\)\\\>")
            (setq going nil)
          ;; not suitable, try again
          (setq first-time nil)
          (setq prev (point))
          ))
       ((eq c ?\;)
        (if first-time
            (progn
              (setq first-time nil)
              (setq prev (point)))
          (goto-char prev)
          (setq going nil)))
       ((eq c ?\n)
        (forward-char 1)
        (setq going (script-awk-backup-over-nl first-time))
        (if (not going)
            (skip-chars-forward " \t"))
        ;; ? (setq first-time nil)
        ;; Note: we do not set  (setq prev (point))  here!
        )
       ((eq cs ?\()
        ;; Check this - is this correct?
        (if first-time
            (progn
              (setq first-time nil)
              (setq prev (point)))
          (goto-char prev)
          (setq going nil))
        )
       (t
        (setq first-time nil)
        (setq prev (point))
        )
       );; cond
      );; while
    (/= start (point))
    );; let
  )

(defun script-awk-prev-stmt ()
  (interactive)
  (let ( save )
    (if (script-awk-prev-stmt-1)
        (progn
          (setq save (point))
          (or (script-awk-at-block-kw t)
              (script-awk-find-containing-stmt))
          (point))
      nil)
    ))


(defun script-awk-in-block-exp (&optional accept-else)
  (save-excursion
    (skip-chars-backward " \t") 
    (if (and 
         (eq (preceding-char) ?\n)
         (script-awk-backup-over-nl t)
         (skip-chars-backward " \t")) 
        (cond
         ((eq (preceding-char) ?\))
          (backward-sexp 1)
          (skip-chars-backward " \t\n")
          (skip-chars-backward "a-z")
          (script-awk-at-block-kw accept-else))
         (accept-else
          (and (forward-word -1)
               (looking-at "else\\>")))
         (t
          nil))
      nil)
    ))


(defun script-awk-find-containing-stmt ()
  ;; Assuming we are at the beginning of a stmt,
  ;; if it is part of an if/for/while/else then goto that
  ;; and return new (point),  else stay and return nil.
  (interactive)
  (let ( (save (point)) (moved nil) )
    (skip-chars-backward " \t") 
    (if (eq (preceding-char) ?\n)
        (progn
          (if (script-awk-backup-over-nl t)
              (progn
                (skip-chars-backward " \t")
                (setq moved t)))))
    (if (eq (preceding-char) ?\))
        (progn
          (backward-sexp 1)
          (setq moved t)
          (skip-chars-backward " \t")))
    (skip-chars-backward "a-z")
    ;; next is looking at if while for or else
    (if (and moved (script-awk-at-block-kw t))
        (point);; success, return (point)
      ;; fail,  return to original and return nil
      (goto-char save)
      nil)
    ))

(define-derived-mode script-awk-mode
  script-mode "script awk"
  "Major mode for awk (gawk, nawk) scripts.
\\{script-awk-mode-map}
Turning on this mode calls the value of the variables `script-mode-hook'
and `script-awk-mode-hook' if these values are non-nil.

Variables controlling indentation style:
  script-default-indent
    This is the default value of indentation except for continuation
    lines.

  script-offset-for-contin
     This controls the indentation of a continuation line,  as 
     determined by script-continued-regexp.

  script-comments-var
     This controls the indentation of comments.  For details, 
     type \\[describe-variable] script-comments-var

  script-auto-indent
     If non nil,  a newline reindents current line and indents
     for the new empty line.
"
  (setq script-tables (append
                       ;; awk specific tables
                       script-awk-table
                       ;; script-basic-table not used for awk
                       ))
  (setq mode-name "awk Script")
  (setq script-continued-regexp script-awk-continued-regexp)


  (run-hooks 'script-awk-mode-hook)
  )

;; for testing
(defun script-toggle-debug ()
  (interactive)
  (setq script-debug (not script-debug))
  (message (format "script-debug is %s" (prin1-to-string script-debug)))
  )


(provide 'script)
;; end of script.el

