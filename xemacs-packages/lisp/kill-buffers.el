;;Someone in gnu.emacs.help asked for a way to kill many buffers at once.

;;Below is a small package containing some functions to kill all buffers
;;of a certain mode.
;;Currently it contains only the functions for the modes I use, but
;;it is straightforward to add additional ones for other modes.

;;It works for emacs 20.3 and 20.4.  I'm not sure about 19.xx, since I have no
;;way to test it.

;;I use it with the following setup in my site-lisp.

;;(require 'kill-buffers)

;;(global-set-key "\C-ck" nil)
;;(global-set-key "\C-cks" 'killb-saved-invisible-file-buffers)
;;(global-set-key "\C-ckt" 'killb-temporary-buffers)
;;(global-set-key "\C-cka" 'killb-all-unnecessary-buffers)
;;(global-set-key "\C-ckb" 'killb-saved-invisible-bibtex-buffers)
;;(global-set-key "\C-cke" 'killb-saved-invisible-emacs-lisp-buffers)
;;(global-set-key "\C-ckh" 'killb-saved-invisible-html-buffers)
;;(global-set-key "\C-ckl" 'killb-saved-invisible-latex-buffers)
;;(global-set-key "\C-ckn" 'killb-saved-invisible-notes-buffers)
;;(global-set-key "\C-ckp" 'killb-saved-invisible-pascal-buffers)
;;(global-set-key "\C-ckr" 'killb-saved-invisible-rmail-buffers)
;;(global-set-key "\C-ckd" 'killb-invisible-dired-buffers)
;;(global-set-key "\C-x4k" 'killb-buffer-other-window)
;;(global-set-key "\C-ckf" 'killb-buffer-delete-frame)
;;(global-set-key "\C-ckk" 'killb-save-buffer-delete-frame)
;;;; included in Emacs 20
;;(global-set-key "\C-ckw" 'kill-buffer-and-window) ; already bound to "C-x 4 0"


;;- Diego


;;; kill-buffers.el --- Various functions to kill buffers by mode

;; Copyright (C) 1997,1998,1999 Diego Calvanese

;; Author: Diego Calvanese <calvanese@dis.uniroma1.it>
;; Maintainer: Diego Calvanese <calvanese@dis.uniroma1.it>
;; Created: 1997
;; Version: 1.1
;; Keywords: kill-buffers

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from
;; the Free Software Foundation, Inc.,
;; 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Code:

(defvar killb-non-temporary-buffers
  '("*Messages*" "*scratch*" "*mail*" "*VM-mail*" "*Buffer List*"
    "*buffer-selection*")
  "*List of buffers that are not considered temporary.")

(defun killb-saved-invisible-file-buffers (&optional vis)
  "Kill all saved file buffers that are not visible.
With optional prefix argument non-nil kill also visible file buffers."
  (interactive "P")
  (let ((buffers (buffer-list))
        buffer)
    (while buffers
      (setq buffer (car buffers)
            buffers (cdr buffers))
      (and (buffer-file-name buffer)
           (not (buffer-modified-p buffer))
           (or vis (not (get-buffer-window buffer 'visible)))
           (kill-buffer buffer)))))

(defun killb-temporary-buffers (&optional vis)
  "Kill all buffers that are not visible whose name starts and ends with *,
except those in `non-temporary-buffers'.
With optional prefix argument non-nil kill also visible buffers."
  (interactive "P")
  (let ((buffers (buffer-list))
        buffer name)
    (while buffers
      (setq buffer (car buffers)
            name (buffer-name buffer)
            buffers (cdr buffers))
      (and (string-match "^\\*.*\\*\\(<[0-9]*>\\)?$" name)
           ;; (eq (aref name 0) ?*)
           ;; (eq (aref name (1- (length name))) ?*)
           (not (member name killb-non-temporary-buffers))
           (or vis (not (get-buffer-window buffer 'visible)))
           (kill-buffer buffer)))))

(defun killb-all-unnecessary-buffers (&optional vis)
  "Kill all saved file buffers, all dired buffers that are not visible, and
all buffers whose name starts with :*, with the exception of
\"*Messages*\", \"*scratch*\", \"*mail*\", and \"*Buffer List*\".
With optional prefix argument non-nil kill also visible file buffers."
  (interactive "P")
  (killb-saved-invisible-file-buffers vis)
  (killb-invisible-dired-buffers vis)
  (killb-temporary-buffers vis))

(defun killb-saved-invisible-mode-buffers (mode &optional vis)
  "Kill all saved buffers with major mode MODE that are not visible.
With optional prefix argument non-nil kill also visible buffers."
  (save-excursion
    (let ((buffers (buffer-list))
          buffer)
      (while buffers
        (setq buffer (car buffers)
              buffers (cdr buffers))
        (and ;; (buffer-file-name buffer)
             (not (buffer-modified-p buffer))
             (or vis (not (get-buffer-window buffer 'visible)))
             (and (set-buffer buffer) (equal major-mode mode))
             (kill-buffer buffer))))))

(defun killb-saved-invisible-bibtex-buffers (&optional vis)
  "Kill all saved bibtex buffers that are not visible.
With optional prefix argument non-nil kill also visible bibtex buffers."
  (interactive "P")
  (killb-saved-invisible-mode-buffers 'bibtex-mode vis))

(defun killb-saved-invisible-emacs-lisp-buffers (&optional vis)
  "Kill all saved emacs-lisp buffers that are not visible.
With optional prefix argument non-nil kill also visible emacs-lisp buffers."
  (interactive "P")
  (killb-saved-invisible-mode-buffers 'emacs-lisp-mode vis))

(defun killb-saved-invisible-html-buffers (&optional vis)
  "Kill all saved buffers in html-helper mode that are not visible.
With optional prefix argument non-nil kill also visible pascal buffers."
  (interactive "P")
  (killb-saved-invisible-mode-buffers 'html-helper-mode vis))

(defun killb-saved-invisible-latex-buffers (&optional vis)
  "Kill all saved latex buffers that are not visible.
With optional prefix argument non-nil kill also visible latex buffers."
  (interactive "P")
  (killb-saved-invisible-mode-buffers 'latex-mode vis))

(defun killb-saved-invisible-notes-buffers (&optional vis)
  "Kill all saved notes buffers that are not visible.
With optional prefix argument non-nil kill also visible notes buffers."
  (interactive "P")
  (killb-saved-invisible-mode-buffers 'notes-mode vis)
  (killb-saved-invisible-mode-buffers 'notes-index-mode vis))

(defun killb-saved-invisible-pascal-buffers (&optional vis)
  "Kill all saved pascal buffers that are not visible.
With optional prefix argument non-nil kill also visible pascal buffers."
  (interactive "P")
  (killb-saved-invisible-mode-buffers 'pascal-mode vis))

(defun killb-saved-invisible-rmail-buffers (&optional vis)
  "Kill all saved rmail buffers that are not visible.
With optional prefix argument non-nil kill also visible rmail buffers."
  (interactive "P")
  (if (boundp 'rmail-summary-buffer)
      (save-excursion
	(let ((buffers))
	  ;; we need to first detect the buffers to kill, ...
	  (mapcar
	   (function
	    (lambda (buffer)
	      (set-buffer buffer)
	      (and (eq major-mode 'rmail-mode)
                   (not (buffer-modified-p buffer))
                   (or vis
                       (not (or (get-buffer-window buffer 'visible)
                                (and rmail-summary-buffer
                                     (get-buffer-window rmail-summary-buffer
                                                        'visible)))))
                   (setq buffers (cons buffer buffers)))))
	   (buffer-list))
	  ;; .. then we kill them
	  (mapcar 'kill-buffer buffers)))))

(defun killb-invisible-dired-buffers (&optional vis)
  "Kill all dired buffers that are not visible.
With optional prefix argument non-nil kill also visible dired buffers."
  (interactive "P")
  (if (boundp 'dired-buffers)
      (let ((buffers dired-buffers)
            buffer)
        (while buffers
          (setq buffer (cdar buffers)
                buffers (cdr buffers))
          (if (or vis (not (get-buffer-window buffer 'visible)))
              (kill-buffer buffer))))))

(defun killb-buffer-other-window ()
  "Kill the buffer in the other window and delete the window."
  (interactive)
  (if (one-window-p t)
      (error "No other window")
    (save-selected-window
      (other-window 1)
      (kill-buffer-and-window))))

(defun killb-buffer-delete-frame ()
  "For server buffers call `server-edit' or `gnuserv-edit'.
For regular buffers, kill the buffer and close it's frame."
  (interactive)
  ;; call server-edit or gnuserv-edit for server buffer
  ;; just kill buffer and frame for regular buffers
  (cond
   ((and (fboundp 'gnuserv-buffer-clients)
         (gnuserv-buffer-clients (current-buffer)))
    (gnuserv-edit))
   ((and (boundp 'server-buffer-clients) server-buffer-clients)
    (server-edit))
   (t
    (if (kill-buffer nil)
	(or (equal (selected-frame) default-minibuffer-frame)
	    (delete-frame))))))

(defun killb-save-buffer-delete-frame ()
  "For server buffers call `server-edit' or `gnuserv-edit'.
For regular buffers, prompt for file to be saved and then kill the buffer
and close it's frame."
  (interactive)
  ;; call server-edit or gnuserv-edit for server buffer
  ;; just kill buffer and frame for regular buffers
  (cond
   ((and (fboundp 'gnuserv-buffer-clients)
         (gnuserv-buffer-clients (current-buffer)))
    (gnuserv-edit))
   ((and (boundp 'server-buffer-clients) server-buffer-clients)
    (server-edit))
   (t
    ;; prompt to save buffer if modified
    (if (and (buffer-modified-p)
	     buffer-file-name
	     (y-or-n-p (concat "Save file " buffer-file-name "? ")))
	(save-buffer nil))
    (if (kill-buffer nil)
	(or (equal (selected-frame) default-minibuffer-frame)
	    (delete-frame))))))

(provide 'kill-buffers)

;;; kill-buffers.el ends here
