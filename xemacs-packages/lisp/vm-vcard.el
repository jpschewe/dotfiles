;;; vm-vcard.el --- vcard parsing and formatting routines for VM

;; Copyright (C) 1997 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Keywords: extensions
;; Created: 1997-10-03

;; $Id: vm-vcard.el,v 1.1 2003/11/29 21:05:17 jpschewe Exp $

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

;;; Commentary:

;; This program contains some sample function for formatting vcards.
;; Feel free to write your own.

;;; Code:

(require 'vcard)

(defvar vm-vcard-format-function 'vcard-format-box) ;vm-vcard-format-simple
(defvar vm-vcard-filter 'vcard-standard-filter)

(defun vm-mime-display-internal-text/x-vcard (layout)
  (let ((inhibit-read-only t)
        (buffer-read-only nil))
    (insert (vm-vcard-format-layout layout)))
  t)

(defun vm-vcard-format-layout (layout)
  (let* ((beg (vm-mm-layout-body-start layout))
         (end (vm-mm-layout-body-end layout))
         (buf (if (markerp beg) (marker-buffer beg) (current-buffer)))
         (raw (vm-vcard-decode (save-excursion
                                 (set-buffer buf)
                                 (save-restriction
                                   (widen)
                                   (buffer-substring beg end)))
                               layout)))
    (funcall vm-vcard-format-function
             (vcard-parse-string raw vm-vcard-filter))))

(defun vm-vcard-decode (string layout)
  (let ((buf (generate-new-buffer " *vcard decoding*")))
    (save-excursion
      (set-buffer buf)
      (insert string)
      (vm-mime-transfer-decode-region layout (point-min) (point-max))
      (setq string (buffer-substring (point-min) (point-max))))
    (kill-buffer buf))
  string)

(defun vm-vcard-format-simple (vcard-data)
  (concat "\n\n--\n" (vcard-display-string vcard-data) "\n\n"))

(provide 'vm-vcard)

;;; vm-vcard.el ends here.
