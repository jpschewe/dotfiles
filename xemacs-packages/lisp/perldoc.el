;;; perldoc.el --- Show help for Perl functions, builtins, and modules.

;;
;; Copyright (C) 2000-2002 Steve Kemp <skx@tardis.ed.ac.uk>
;;

;; This file is not part of GNU Emacs.

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

;;; Commentary:
;;

;;  This package allows the user to view the Perl help for the
;; word(s) at the point.
;;
;;  When this is loaded it adds a hook both `cperl-mode', and
;; `perl-mode', allowing the perldoc help to be shown for the
;; thing under the point, by pressing F1.
;;
;;  The code handles functions, builtins, and third party
;; modules.
;;

;;; Version History
;;
;;  1.0 - Initial Release.
;;  1.1 - Show error message when no help is found.
;;      - Fix name.
;;      - Include GPL + URL.
;;

;;; Source
;;
;;  The latest version of this file should be accessable from:
;;
;;  http://GNUSoftware.com/Emacs/Lisp/perldoc.el
;;
;;  Comments / suggests / feedback welcomed to skx@tardis.ed.ac.uk


(require 'thingatpt)

(defvar perldoc-functions 
  (list
   "-X"
   "abs"
   "accept"
   "alarm"
   "atan2"
   "bind"
   "binmode"
   "bless"
   "caller"
   "chdir"
   "chmod"
   "chomp"
   "chop"
   "chown"
   "chr"
   "chroot"
   "close"
   "closedir"
   "connect"
   "continue"
   "cos"
   "crypt"
   "dbmclose"
   "dbmopen"
   "defined"
   "delete"
   "die"
   "do"
   "dump"
   "each"
   "endgrent"
   "endhostent"
   "endnetent"
   "endprotoent"
   "endpwent"
   "endservent"
   "eof"
   "eval"
   "ex"
   "exec"
   "exists"
   "exit"
   "exp"
   "fcntl"
   "fileno"
   "flock"
   "fork"
   "format"
   "formline"
   "getc"
   "getgrent"
   "getgrgid"
   "getgrnam"
   "gethostbyaddr"
   "gethostbyname"
   "gethostent"
   "getlogin"
   "getnetbyaddr"
   "getnetbyname"
   "getnetent"
   "getpeername"
   "getpgrp"
   "getppid"
   "getpriority"
   "getprotobyname"
   "getprotobynumber"
   "getprotoent"
   "getpwent"
   "getpwnam"
   "getpwuid"
   "getservbyname"
   "getservbyport"
   "getservent"
   "getsockname"
   "getsockopt"
   "glob"
   "gmtime"
   "goto"
   "grep"
   "hex"
   "import"
   "index"
   "int"
   "ioctl"
   "join"
   "keys"
   "kill"
   "last"
   "lc"
   "lcfirst"
   "length"
   "link"
   "listen"
   "local"
   "localtime"
   "log"
   "lstat"
   "map"
   "mkdir"
   "msgctl"
   "msgget"
   "msgrcv"
   "msgsnd"
   "my"
   "next"
   "no"
   "oct"
   "open"
   "opendir"
   "ord"
   "pack"
   "package"
   "pipe"
   "pop"
   "pos"
   "print"
   "printf"
   "prototype"
   "push"
   "qr"
   "quotemeta"
   "qw"
   "qx"
   "rand"
   "read"
   "readdir"
   "readline"
   "readlink"
   "readpipe"
   "recv"
   "redo"
   "ref"
   "rename"
   "require"
   "reset"
   "return"
   "reverse"
   "rewinddir"
   "rindex"
   "rmdir"
   "scalar"
   "seek"
   "seekdir"
   "select"
   "semctl"
   "semget"
   "semop"
   "send"
   "setgrent"
   "sethostent"
   "setnetent"
   "setpgrp"
   "setpriority"
   "setprotoent"
   "setpwent" 
   "setservent" 
   "setsockopt"
   "shift"
   "shmctl"
   "shmget"
   "shmread"
   "shmwrite"
   "shutdown"
   "sin"
   "sleep"
   "socket"
   "socketpair"
   "sort"
   "splice"
   "split"
   "sprintf"
   "sqrt"
   "srand"
   "stat"
   "study"
   "sub"
   "substr"
   "symlink"
   "syscall"
   "sysopen"
   "sysread"
   "sysseek"
   "system"
   "syswrite"
   "tell"
   "telldir"
   "tie"
   "tied"
   "time"
   "times"
   "times" 
   "truncate"
   "uc"
   "ucfirst"
   "umask"
   "undef"
   "unlink"
   "unpack"
   "unshift"
   "untie"
   "use"
   "utime"
   "values"
   "vec"
   "wait"
   "waitpid"
   "wantarray"
   "warn"
   "write"
   )
  "Perl function names."
  )


(defun perldoc ( string )
  "Run perldoc on the given string.
If the string is a recognised function then we cann call `perldoc-function',
otherwise we call `perldoc-module'."
  (interactive "s:Perl function / module: " )
  (let ((functions perldoc-functions)
	(found nil ))
    (while (and (car functions) (not found))
      (if (equal string (car functions))
	  (progn
	    (perldoc-function string)
	    (setq found t)))
      (setq functions (cdr functions)))
    (if (not found)
	(perldoc-module string)))
)

(defun perldoc-function( function )
 "Show the help text for the given Perl function / builtin."
 (interactive "sPerl function / builtin / module : ")
 (let ((perldoc-process nil))
   (set-buffer (get-buffer-create "*Perldoc*"))
   (kill-all-local-variables)
   (erase-buffer)
   (text-mode)
   (message "Loading documentation ..")
   (setq perldoc-process (start-process "perldol" nil "perldoc" "-f" function))
   (set-process-filter perldoc-process 'perldoc-process-filter)
   (set-process-sentinel perldoc-process 'perldoc-sentinel)
   (process-kill-without-query perldoc-process)
   ))

(defun perldoc-module( module )
 "Show the help text for the given Perl module."
 (interactive "sPerl module : ")
 (let ((perldoc-process nil))
   (set-buffer (get-buffer-create "*Perldoc*"))
   (kill-all-local-variables)
   (erase-buffer)
   (text-mode)
   (message "Loading documentation ..")
   (setq perldoc-process (start-process "perldol" nil "perldoc" module))
   (set-process-filter perldoc-process 'perldoc-process-filter)
   (set-process-sentinel perldoc-process 'perldoc-sentinel)
   (process-kill-without-query perldoc-process)
   ))

(defun perldoc-process-filter (proc string)
  "Process the results from the catdoc process."
  (set-buffer (get-buffer-create "*Perldoc*"))
  (insert string))

(defun perldoc-sentinel (proc msg)
  "When the catdoc process has finished, switch to its output buffer,
 and rename it appropriately."
  (cond ((eq (process-status proc) 'exit)
	 (set-buffer "*Perldoc*")
	 (if (< (count-lines (point-min) (point-max)) 2)
	     (progn
	       (message "No perdoc help found.")
	       (kill-buffer (get-buffer "*Perldoc*")))
	   (pop-to-buffer "*Perldoc*")
	   (goto-char (point-min)))
	 )))


(defun perldoc-cperl-hook ()
  "A hook which binds F1 to `perldoc'."
  (local-set-key [f1] '(lambda ()
			   (interactive)
			   (perldoc (thing-at-point 'filename)))
		 )
)

(defun perldoc-perl-hook ()
  "A hook which binds F1 to `perldoc'."
  (local-set-key [f1] '(lambda ()
			   (interactive)
			   (perldoc (thing-at-point 'filename)))
		 )
)

(add-hook 'cperl-mode-hook 'perldoc-cperl-hook)
(add-hook 'perl-mode-hook 'perldoc-perl-hook)

(provide 'perldoc)

;;; perldoc.el ends here
