;;; complete-message-recipient.el --- tab mail and news recipient completion

;;; Copyright (C) 1995, 1996 Aaron Larson, Eric Engstrom
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to kyle@uunet.uu.net) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;;
;;; Send bug reports to engstrom@htc.honeywell.com

;;; Do line wrapping in mail/news headers with a leading tab, and also
;;; provide for completion of mail aliases and news group names with tab.
;;;
;;; Unfortunately, they are highly dependent upon the gnus and mail modes,
;;; as well as your local mail configuration, and may not work w/out
;;; modification for your environment.
;;;
;;; We have used this under XEmacs 19.13 and GNU Emacs 19.30

;;; emacs 19.25 could probably use text properties
;;; Add-text-properties...
;;; lucid emacs, extent-property 

;; Where is the header separator?
(defvar mail-header-separator-position nil)
(make-variable-buffer-local 'mail-header-separator-position)

(defun mail-header-separator-position ()
  (or mail-header-separator-position
      (save-excursion
	(goto-char (point-min))
	;; [tap:19960626.1531CST] Return 0 if no header.
	(if (search-forward mail-header-separator (point-max) t)
	    (progn
	      (beginning-of-line)
	      (setq mail-header-separator-position (set-marker (make-marker) (point))))
	  0))))

;; do line wrapping and fill header lines with leading tab
(defun message-header-auto-fill ()
  "Do an auto-fill in the header of a mail message."
  (let ((fill-prefix (if (position-in-header-p (point))
			 "\t"
		       fill-prefix)))
    (do-auto-fill)))
    
;; What header is under point
(defun complete-recipient-header-at-point (&optional point)
  (save-excursion
    (and point (goto-char point))
    (beginning-of-line nil)
    (if (not (looking-at "^\t"))
        ;(foobar-word "_a-zA-Z---.0-9:")
	(current-word)
      (beginning-of-line -1)
      (complete-recipient-header-at-point))))

(defun position-in-header-p (&optional point)
  (< (or point (point)) (mail-header-separator-position)))

(defvar complete-recipient-mail-header-regexp
  "^\\(To\\|CC\\|BCC\\|Reply-To\\)")

;; are we on a line looking for a mail address
(defun position-in-header-expecting-mail-alias-p (&optional point)
  (and (position-in-header-p point)
       (string-match complete-recipient-mail-header-regexp
		     (complete-recipient-header-at-point (or point (point))))))

(defvar complete-recipient-newgroups-header-regexp
  "^\\(Newsgroups\\|Followup-To\\|Posted-To\\)")

;; are we on a line looking for a newsgroup
(defun position-in-header-expecting-newsgroup-p (&optional point)
  (and (position-in-header-p point)
       (string-match complete-recipient-newgroups-header-regexp
		     (complete-recipient-header-at-point (or point (point))))))

;; This is a hack.  Perhaps we could do something with extents.
(defvar complete-recipient-tab-command nil)
(make-variable-buffer-local 'complete-recipient-tab-command)

(defun complete-message-recipient-mode ()
  "Provide completion for mail aliases and news group names in the current
buffer's header area"
  (setq complete-recipient-tab-command (lookup-key (current-local-map) "\t"))
  (local-set-key "\t" 'complete-message-recipient-or-tab)
  (setq auto-fill-function 'message-header-auto-fill))

;(defvar complete-both-mail-and-news-recipients nil
;  "When completing mail-aliases, allow newsgroups to be completed as well.")
(defvar complete-both-mail-and-news-recipients t
  "When completing mail-aliases, allow newsgroups to be completed as well.")

(defun complete-message-recipient-or-tab ()
  "If in the header of a mail message, complete the mail alias preceeding the
point; If in the header of a news message, complete the news group
preceeding the point; otherwise do 'standard' tab command."
  (interactive)
  (cond ((position-in-header-expecting-mail-alias-p (point))
	 (if complete-both-mail-and-news-recipients
	     (complete-mail-or-news-recipient)
	   (complete-mail-recipient)))
	((position-in-header-expecting-newsgroup-p (point))
	 (complete-newsgroup-recipient))
	(t
	 (funcall complete-recipient-tab-command))))

;; What are legal chars in a mail addr anyway?
(defun complete-mail-recipient ()
    (insert-completion (foobar-word "_a-zA-Z---.0-9%") (mail-all-aliases-alist)))

(defun complete-mail-or-news-recipient ()
  (let ((list-a (mail-all-aliases-alist))
	(list-b (complete-newsgroup-newsrc-alist)))
    (let ((last-a (last list-a)))
      (unwind-protect
	  (progn
	    (setf (cdr last-a) list-b)
	    (insert-completion (foobar-word "_a-zA-Z---.0-9%") list-a))
	(setf (cdr last-a) nil)))))

;; What are legal chars in a newsgroup name anyway?
(defun complete-newsgroup-recipient ()
    (insert-completion (foobar-word "_a-zA-Z---.0-9") (complete-newsgroup-newsrc-alist)))

(autoload 'gnus-make-newsrc-file "gnus-start")
(defun complete-newsgroup-newsrc-alist ()
  (if (or (not (boundp 'gnus-newsrc-alist))
	  (not gnus-newsrc-alist))
    (progn
      (message "Building newsgroup list...")
      ;; Code fragments scarfed from gnus.el (gnus-setup-news)
      (setq gnus-current-startup-file
	    (gnus-make-newsrc-file gnus-startup-file))
      (gnus-read-newsrc-file)
      (message "Building newsgroup list...Done")))
  gnus-newsrc-alist)

(defvar mail-alias-list-command
  "ypcat -k aliases | awk '{print $1}'; test -e $HOME/.mailrc && awk '/^[ \t]*alias/{print $2}' $HOME/.mailrc"
  "*A shell command that will list all the mail aliases, one per line.")

(defun mail-alias-list-command ()
  (or mail-alias-list-command
      (let ((mailrc (or (getenv "MAILRC")
			(expand-file-name "$HOME/.mailrc"))))
	(format "ypcat -k aliases | awk '{print $1}'; test -e %s && awk '/^[ \t]*alias/{print $2}' %s"
		mailrc mailrc))))

(defvar %mail-all-aliases-alist% nil)
(defun mail-all-aliases-alist ()
  (or %mail-all-aliases-alist%
      (setq %mail-all-aliases-alist%
	    (save-excursion
	      (message "Building mail alias table...")
	      (set-buffer (get-buffer-create "*mail-complete*"))
	      (delete-region (point-min) (point-max))
	      (call-process shell-file-name nil t nil "-c" mail-alias-list-command)
	      (prog1
		  (mapcar (function (lambda (x) (cons x nil)))
			  (buffer=>list-of-strings))
		(kill-buffer (current-buffer))
		(message "Building mail alias table...Done"))))))

(defun buffer=>list-of-strings ()
  (save-excursion
    (goto-char (point-min))
    (let ((l nil))
      (while (not (eobp))
	(setq l (cons (buffer-substring (point) (save-excursion (end-of-line) (point)))
		      l))
	(forward-line 1))
      (nreverse l))))

;; (require 'comint)
;; Need a general solution for this one; this one used to be called comint-word
(defun foobar-word (word-chars)
  "Return the word of WORD-CHARS at point, or an empty string if none is found.
Word constituents are considered to be those in WORD-CHARS, which is like the
inside of a \"[...]\" (see `skip-chars-forward')."
  (save-excursion
    (let ((limit (point))
	  (word (concat "[" word-chars "]"))
	  (non-word (concat "[^" word-chars "]")))
      (if (re-search-backward non-word nil 'move)
	  (forward-char 1))
      ;; Anchor the search forwards.
      (if (or (eolp) (looking-at non-word))
	  ""
	(re-search-forward (concat word "+") limit)
	(buffer-substring (match-beginning 0) (match-end 0))))))

(defun insert-completion (pattern alist &optional predicate mapfun)
  "Insert into the current buffer the completion of PATTERN, which is assumed
to be the same as the text immediately preceeding point.  If there are 
multiple completions, they are displayed via display-completion-list.  
Optional argument MAPFUN, if non nil is applied to each potential completion,
ala mapcar, prior to calling display-completion-list.  Return values is as
for try-completion, except that when nil would have been returned, an error
is signaled instead."
  (let ((try-completion-result (try-completion pattern alist predicate)))
    (cond ((eq try-completion-result t)
	   (remove-completions-window))
	  ((null try-completion-result)
	   (message "Can't find completion for \"%s\"" pattern)
	   (ding))
	  ((not (string= pattern try-completion-result))
	   (delete-region (point) (- (point) (length pattern)))
	   (insert try-completion-result)
	   (remove-completions-window))
	  (t
	   (message "Making completion list...")
	   (let ((list (all-completions pattern alist predicate)))
	     (if mapfun
		 (setq list (mapcar mapfun list)))	     
	     (with-output-to-temp-buffer "*Completions*"
	       (display-completion-list list)))
	   (message "Making completion list...%s" "done")))
    try-completion-result))

(defun remove-completions-window ()
  (let  (buf win)
    (if (and (setq buf (get-buffer "*Completions*"))
	     (setq win (get-buffer-window buf)))
	(progn
	  ;(message "hello")
	  (delete-window win)))))

;;;
