;;; semantic-grammar-wy.el --- Generated parser support file

;; Copyright (C) 2002, 2003, 2004 David Ponce

;; Author: Jon Schewe <jpschewe@workstation.mn.mtu.net>
;; Created: 2005-07-22 06:48:18z
;; Keywords: syntax
;; X-RCS: $Id$

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; PLEASE DO NOT MANUALLY EDIT THIS FILE!  It is automatically
;; generated from the grammar file semantic-grammar.wy.

;;; History:
;;

;;; Code:

;;; Prologue
;;

;;; Declarations
;;
(defconst semantic-grammar-wy--keyword-table
  (semantic-lex-make-keyword-table
   'nil
   'nil)
  "Table of language keywords.")

(defconst semantic-grammar-wy--token-table
  (semantic-lex-make-type-table
   'nil
   'nil)
  "Table of lexical tokens.")

(defconst semantic-grammar-wy--parse-table
  (progn
    (eval-when-compile
      (require
       'wisent-comp))
    (wisent-compile-grammar
     '(nil nil)
     'nil))
  "Parser table.")

(defun semantic-grammar-wy--install-parser ()
  "Setup the Semantic Parser."
  (semantic-install-function-overrides
   '((parse-stream . wisent-parse-stream)))
  (setq semantic-parser-name "LALR"
	semantic--parse-table semantic-grammar-wy--parse-table
	semantic-debug-parser-source "semantic-grammar.wy"
	semantic-flex-keywords-obarray semantic-grammar-wy--keyword-table
	semantic-lex-types-obarray semantic-grammar-wy--token-table)
  ;; Collect unmatched syntax lexical tokens
  (semantic-make-local-hook 'wisent-discarding-token-functions)
  (add-hook 'wisent-discarding-token-functions
	    'wisent-collect-unmatched-syntax nil t))


;;; Analyzers
;;
(require 'semantic-lex)


;;; Epilogue
;;

(provide 'semantic-grammar-wy)

;;; semantic-grammar-wy.el ends here
