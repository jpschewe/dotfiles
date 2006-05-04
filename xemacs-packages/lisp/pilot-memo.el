;;; pilot-memo.el - write the current buffer out to ones' Palm device as a memo.
;;
;; Copyright (C) 1999 Andrew J Cosgriff
;; http://polydistortion.net/sw/emacs-lisp/
;;
;; Author: Andrew J Cosgriff <ajc@bing.wattle.id.au>
;; Created: Wed Aug 18 22:31:35 1999
;; Version: $Id: pilot-memo.el 2 2003-11-29 21:05:18Z jpschewe $
;; Keywords: palm pilot memo
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Inspired by David Maslen :)
;;
;; Usage : M-x save-buffer-to-pilot
;;
;; It prompts for the title of the memo.
;;
;;; TO DO:
;;
;; - ask what category to chuck it into ? we set a default now, at
;;   least
;;
;; - add hooks for Gnus (done) and maybe w3 to pick a reasonable
;;   default for the title.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; History:
;;
;; 1.1 - Initial version
;;
;; 1.5 - variables are customizable
;;       - allow setting of category to put it in (at customize-time,
;;         anyway)
;;       - Gnus binding - M-x gnus-summary-save-article-pilot
;;         (what to bind this to ?)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst pilot-memo-version (substring "$Revision: 1.1 $" 11 -2)
  "Version of pilot-memo.")

;;
;;; User Customizable Variables:
;;
(defgroup pilot-memo nil
  "Upload emacs buffers to your Palm device.")

(defcustom pilot-memo-category "Emacs"
  "Default category to upload memos into."
  :type 'string
  :group 'pilot-memo)

(defcustom pilot-memo-install-memo-program "install-memo"
  "Program to run that will install the memo onto your Palm device."
  :type 'string
  :group 'pilot-memo)

(defcustom pilot-memo-device (or (getenv "PILOTPORT") "/dev/ttyS0")
  "Device name for the serial port to which your Palm's hotsync cable is connected."
  :type 'string
  :group 'pilot-memo)

;;
;;; Code:
;;

(defun pilot-get-temp-dir ()
  (cond ((boundp 'temp-directory)
	 temp-directory)
	((boundp 'temporary-file-directory)
	 temporary-file-directory)
	(t ".")))

(defun save-buffer-to-pilot (title)
  (interactive (list (read-string "memo title: " (buffer-name))))
  (let ((pilot-memo-buffer (get-buffer-create "*pilot-memo*"))
	(pilot-memo-filename (concat (pilot-get-temp-dir) "/pilot-memo.txt"))
	(pilot-memo-out-buffer (get-buffer-create "*Shell Command Output*")))
    (save-excursion
      (copy-region-as-kill (point-min) (point-max))
      (set-buffer pilot-memo-buffer)
      (erase-buffer)
      (insert title "\n")
      (yank)
      (delete-windows-on pilot-memo-buffer)
      (write-file pilot-memo-filename nil))
    (kill-buffer pilot-memo-buffer)
    (message "Press the hotsync button...")
    (shell-command (concat pilot-memo-install-memo-program " -q -c \"" pilot-memo-category "\" -p " pilot-memo-device " " pilot-memo-filename))
;;    (set-buffer pilot-memo-out-buffer)
;;    (if (<= 2 (count-lines (point-min) (point-max)))
;;	(progn
;;	  (message "memo installed !")
;;	  (pop-to-buffer pilot-memo-out-buffer)
;;	  (local-set-key "q" (lambda () (interactive) (and (not (one-window-p)) (delete-window) (kill-buffer pilot-memo-out-buffer))))
;;	  (shrink-window (- (window-displayed-height) (count-lines (point-min) (point-max))))))
    (delete-file pilot-memo-filename)))

(defun gnus-summary-save-article-pilot (&optional arg)
  "Save the current article to a memo on your Palm device.
If N is a positive number, save the N next articles.
If N is a negative number, save the N previous articles.
If N is nil and any articles have been marked with the process mark,
save those articles instead."
  (interactive "P")
  (let ((gnus-default-article-saver 'gnus-summary-save-in-pilot-memo))
    (gnus-summary-save-article arg)))

(defun gnus-summary-save-in-pilot-memo (&optional filename)
  "Save this article to a memo on your Palm device.
Optional argument FILENAME specifies file name."
  (gnus-eval-in-buffer-window gnus-save-article-buffer
    (save-buffer-to-pilot (read-string "memo title: " (mail-header-subject gnus-current-headers)))))

;;; pilot-memo.el ends here
