;; @(#) extra-java-font-lock.el -- extra highlighting for java
;; @(#) $Id: extra-java-font-lock.el,v 1.1 2003/11/29 21:05:18 jpschewe Exp $

;; This file is not part of Emacs

;; Copyright (C) 1998, 1999 by David Ponce
;; Author:       David Ponce david.ponce@wanadoo.fr
;; Maintainer:   David Ponce david.ponce@wanadoo.fr
;; Created:      September 28 1998

;; LCD Archive Entry:
;; extra-java-font-lock|David Ponce|david.ponce@wanadoo.fr|
;; extra highlighting for java|
;; $Date: 2003/11/29 21:05:18 $|$Revision: 1.1 $|~/misc/extra-java-font-lock.el|

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Description:
;;
;;  Adds some extra highlighting to `java-mode' and `jde-mode'

;;; Installation:
;;
;;  Put this file on your Emacs-Lisp load path and add following into your
;;  ~/.emacs startup file
;;
;;     (cond ((fboundp 'global-font-lock-mode)
;;           (require 'extra-java-font-lock)
;;           (setq font-lock-maximum-decoration t)
;;           (global-font-lock-mode t)
;;           ))

;;; Usage:
;;

;;; Customization:
;;

;;; Support:
;;
;;  Any comments, suggestions, bug reports or upgrade requests are welcome.
;;  Please send them to David Ponce at david.ponce@wanadoo.fr.
;;
;;  This version of jpack was developed with NTEmacs 20.3.1 under MS Windows
;;  NT 4 WKS SP3 and also tested with Emacs 20.3 under Sun Solaris 2.5.
;;  Please, let me know if it works with other OS and versions of Emacs.

;;; Code:

;; java-font-lock20a.el --- patch font-lock for Java in Gnu Emacs20
;;(load "java-font-lock20a")

;; creates a specific face for numbers
(defface extra-java-font-lock-number-face
  '((((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (((class color) (background light)) (:foreground "RosyBrown"))
    (((class color) (background dark)) (:foreground "LightSalmon"))
    (t (:italic t)))
  "Font Lock mode face used to highlight numbers."
  :group 'font-lock-highlighting-faces)

;; defines the extra font lock faces
(defvar extra-java-font-lock-number-face    'extra-java-font-lock-number-face)
(defvar extra-java-font-lock-bold-face      'bold)
(defvar extra-java-font-lock-italic-face    'italic)
(defvar extra-java-font-lock-underline-face 'underline)
(defvar extra-java-font-lock-pre-face       'default)
(defvar extra-java-font-lock-code-face      'font-lock-builtin-face)

;; extra fontification regexps
(defvar extra-java-font-lock-keywords
  (eval-when-compile
    (list
     
     ;; Use a different face for modifiers
      (cons (concat "\\<\\("
                    (regexp-opt '("abstract" "const" "final" "synchronized"
                                  "transient" "static" "volatile" "public"
                                  "private" "protected" "native"))
                    "\\)\\>")
            'font-lock-builtin-face)
       
     '("\\b\\(0[xX][0-9a-fA-F]+[lL]?\\|[0-9]+\\.?[0-9]*\\([eE][-+]?[0-9]+\\)?\\([lL]\\|[fF]\\|[dD]\\)?\\)\\b" . extra-java-font-lock-number-face)
       
     '("\\b\\(\\.[0-9]+\\([eE][-+]?[0-9]+\\)?\\([lL]\\|[fF]\\|[dD]\\)?\\)\\b" . extra-java-font-lock-number-face)
       
     ;; Fontify capitalised identifiers as constant
;;     '("\\(\\.\\|\\b\\)\\([A-Z_]+[A-Z0-9_]*\\)\\([]-[;,.=><!~?:&|+*/^%) \n\r\t]\\)"
;;       2 font-lock-constant-face             keep)
     '("\\b[A-Z_]+[A-Z0-9_]*\\b" . font-lock-constant-face)

     ;; Fontify text between `' in comments
     '("`\\(.*\\)'"
       1 font-lock-constant-face             prepend)
       
     ;; Basic HTML highlighting in javadoc comments
     ;; Fontify the text of a HREF anchor.
     '("<[Aa]\\s-+[Hh][Rr][Ee][Ff][^>]*>\\([^>]+\\)</[Aa]>"
       1 font-lock-reference-face            t)
     ;; Fontify <b>, <strong>, <i>, <u>, <code> and <pre> tags when no tags inside
     '("<[Ss][Tt][Rr][Oo][Nn][Gg]>\\([^<]*\\)</[Ss][Tt][Rr][Oo][Nn][Gg]>"
       1 extra-java-font-lock-bold-face      t)
     '("<[Bb]>\\([^<]*\\)</[Bb]>"
       1 extra-java-font-lock-bold-face      t)
     '("<[Ii]>\\([^<]*\\)</[Ii]>"
       1 extra-java-font-lock-italic-face    t)
     '("<[Uu]>\\([^<]*\\)</[Uu]>"
       1 extra-java-font-lock-underline-face t)
     '("<[Cc][Oo][Dd][Ee]>\\([^<]*\\)</[Cc][Oo][Dd][Ee]>"
       1 extra-java-font-lock-code-face      t)
     '("<[Pp][Rr][Ee]>\\([^<]*\\)</[Pp][Rr][Ee]>"
       1 extra-java-font-lock-pre-face       t)

     )
    )
  )

;; adds the extra fonctification to java-mode and jde-mode
(font-lock-add-keywords 'java-mode extra-java-font-lock-keywords)
(font-lock-add-keywords 'jde-mode extra-java-font-lock-keywords)

(provide 'extra-java-font-lock)

;;; Change History:

;;
;; $Log: extra-java-font-lock.el,v $
;; Revision 1.1  2003/11/29 21:05:18  jpschewe
;; Initial import.
;;
;; Revision 1.4  1999-08-24 10:31:51+02  ebat311
;; New regexp to highlight capitalised identifiers as constants.
;;
;; Revision 1.3  1999-04-23 00:05:10+02  ebat311
;; FIXED: capitalised identifiers with leading underscore
;;             were not highlighted.
;;
;; Revision 1.2  1999-03-05 14:13:17+01  ebat311
;; Improved regexps.
;;
;; Revision 1.1  1999-02-01 12:25:17+01  ebat311
;; Initial revision
;;
;;

;;; extra-java-font-lock.el ends here.
