;;; pretty-column.el --- Prettify all columns in a region or rectangle.

;; Copyright (C) 1999 Vinicius Jose Latorre <vinicius@cpqd.com.br>

;; Author: Vinicius Jose Latorre <vinicius@cpqd.com.br>
;; Maintainer: Vinicius Jose Latorre <vinicius@cpqd.com.br>
;; Time-stamp:	<99/08/07 13:36:50 vinicius>
;; Version: 1.1
;; Keywords: internal

;; This file is NOT (yet?) part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; pretty-column helps to prettify columns in a text region or rectangle.
;;
;; To use it, make sure that this file is in load-path and insert in your
;; .emacs:
;;
;;    (require 'pretty-column)
;;
;; If you have, for example, the following columns:
;;
;;	a	b	c	d
;;	aaaa	bb	ccc	ddddd
;;	aaa	bbb	cccc   	dddd
;;	aa	bb	ccccccc	ddd
;;
;; And the following settings:
;;
;;    (setq pcol-str-before "[ ")
;;    (setq pcol-str-after " ]")
;;    (setq pcol-str-separator ", ")
;;    (setq pcol-column-separator "\t")
;;
;; If you select the columns above and type:
;;
;;    M-x pretty-column RET
;;
;; You obtain the following result:
;;
;;	[ a   , b  , c      , d     ]
;;	[ aaaa, bb , ccc    , ddddd ]
;;	[ aaa , bbb, cccc   , dddd  ]
;;	[ aa  , bb , ccccccc, ddd   ]
;;
;; But if you select start from the very first b and the very last c and type:
;;
;;    M-x pretty-rectangle RET
;;
;; You obtain the following result:
;;
;;	a	[ b  , c       ]	d
;;	aaaa	[ bb , ccc     ]	ddddd
;;	aaa	[ bbb, cccc    ]	dddd
;;	aa	[ bb , ccccccc ]	ddd
;;
;; Note that `pretty-column' operates over all text region selected, extending
;; the region start to the beginning of line and the region end to the end of
;; line.  While `pretty-rectangle' operates over the text rectangle selected
;; which rectangle diagonal is given by the region start and end.
;;
;; `pretty-column' is useful when you have columns of text that are not well
;; aligned, like:
;;
;;	horse	apple	bus
;;	dog	pineapple	car
;;	porcupine	strawberry	airplane

;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Options:

(defvar pcol-str-before ""
  "*Specify a string to be inserted before all columns.")

(defvar pcol-str-separator ", "
  "*Specify a string to be inserted between each column.")

(defvar pcol-str-after ""
  "*Specify a string to be inserted after all columns.")

(defvar pcol-column-separator "\t"
  "*Specify a regexp which separates each column.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Commands:


;;;###autoload
(defun pretty-column (start end)
  "Prettify all columns in a text region.

START and END delimits the text region."
  (interactive "*r")
  (let ((pcol-str-before    (if (stringp pcol-str-before)
				pcol-str-before
			      ""))
	(pcol-str-separator (if (stringp pcol-str-separator)
				pcol-str-separator
			      " "))
	(pcol-str-after     (if (stringp pcol-str-after)
				pcol-str-after
			      ""))
	(pcol-limit (make-marker))
	(the-end (copy-marker end))
	pcol-max)
    (save-excursion
      (goto-char start)
      (beginning-of-line)
      ;; get maximum length for each column
      (save-excursion
	(while (< (point) the-end)
	  (pretty-rectangle-max
	   (prog1
	       (point)
	     (end-of-line)))
	  (forward-char 1)))
      ;; prettify columns
      (while (< (point) the-end)
	(pretty-rectangle-line
	 (prog1
	     (point)
	   (end-of-line)))
	(forward-char 1))
      ;; nullify markers
      (set-marker pcol-limit nil)
      (set-marker the-end nil))))


(require 'rect)


;;;###autoload
(defun pretty-rectangle (start end)
  "Prettify all columns in a text rectangle.

START and END delimits the corners of text rectangle."
  (interactive "*r")
  (let ((pcol-str-before    (if (stringp pcol-str-before)
				pcol-str-before
			      ""))
	(pcol-str-separator (if (stringp pcol-str-separator)
				pcol-str-separator
			      " "))
	(pcol-str-after     (if (stringp pcol-str-after)
				pcol-str-after
			      ""))
	(pcol-limit (make-marker))
	(the-end (copy-marker end))
	pcol-max)
    ;; get maximum length for each column
    (save-excursion
      (operate-on-rectangle 'pretty-rectangle-max start the-end t))
    ;; prettify columns
    (save-excursion
      (operate-on-rectangle 'pretty-rectangle-line start the-end t))
    ;; nullify markers
    (set-marker pcol-limit nil)
    (set-marker the-end nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal Variables and Functions:


;; to avoid compilation gripes
(defvar pcol-max nil)
(defvar pcol-limit nil)


(defun pretty-rectangle-max (startpos &optional ignore ignore)
  (set-marker pcol-limit (point))
  (goto-char startpos)
  (if pcol-max
      ;; get maximum column length
      (let ((ncol 0)
	    origin)
	(while (progn
		 (setq origin (current-column))
		 (re-search-forward pcol-column-separator pcol-limit 'move))
	  (save-excursion
	    (goto-char (match-beginning 0))
	    (aset pcol-max ncol (max (aref pcol-max ncol)
				     (- (current-column) origin))))
	  (setq ncol (1+ ncol)))
	(aset pcol-max ncol (max (aref pcol-max ncol)
				 (- (current-column) origin))))
    ;; initialize column length vector
    (let ((ncol 1)
	  origin values)
      (while (progn
	       (setq origin (current-column))
	       (re-search-forward pcol-column-separator pcol-limit 'move))
	(save-excursion
	  (goto-char (match-beginning 0))
	  (setq values (cons (- (current-column) origin)
			     values)))
	(setq ncol (1+ ncol)))
      (setq values (cons (- (current-column) origin)
			 values)
	    pcol-max (make-vector ncol 0))
      (while values
	(setq ncol (1- ncol))
	(aset pcol-max ncol (car values))
	(setq values (cdr values))))))


(defun pretty-rectangle-line (startpos &optional ignore ignore)
  (let ((ncol 0)
	origin)
    (set-marker pcol-limit (point))
    (goto-char startpos)
    (insert pcol-str-before)
    (while (progn
	     (setq origin (current-column))
	     (re-search-forward pcol-column-separator pcol-limit 'move))
      (delete-region (match-beginning 0) (point))
      (insert (make-string (- (aref pcol-max ncol)
			      (- (current-column) origin))
			   ?\ )
	      pcol-str-separator)
      (setq ncol (1+ ncol)))
    (insert (make-string (- (aref pcol-max ncol)
			    (- (current-column) origin))
			 ?\ )
	    pcol-str-after)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'pretty-column)


;;; pretty-column.el ends here
