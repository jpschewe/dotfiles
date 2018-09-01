;;; ID: dict.el,v 1.4 1999/07/12 22:32:20 ttn Rel
;;;
;;; Copyright (C) 1999 Thien-Thi Nguyen
;;; This file is part of ttn's personal elisp library, released under GNU
;;; GPL with ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

;;; Description: Consult Merriam-Webster online dictionary or thesaurus.

(require 'thingatpt)

(defun m-w-lookup (book word cutoff)
  (switch-to-buffer (generate-new-buffer
		     (format "*Merriam-Webster %s: %s*" book word)))
  (call-process "lynx" nil t nil "-dump"
		(concat "http://www.m-w.com/cgi-bin/"
			book
			"?book=" (capitalize book)
			"&va=" word))
  (goto-char (point-max))
  ;; kludge
  (while (/= (point) (point-min))
    (when (= 183 (following-char))
      (delete-char 1)
      (insert " "))
    (forward-char -1))
  (when (re-search-forward cutoff (point-max) t)
    (delete-region (point-min) (point))
    (re-search-forward "^\\s-+____+")
    (delete-region (point) (point-max)))
  (goto-char (point-min)))

;;;###autoload
(defun dict (w)
  (interactive (list (read-string "Dictionary Lookup: " (word-at-point))))
  (m-w-lookup "dictionary" w "^Main Entry:\\s-+"))

;;;###autoload
(defun thesaurus (w)
  (interactive (list (read-string "Thesaurus Lookup: " (word-at-point))))
  (m-w-lookup "thesaurus" w "^Entry Word:\\s-+"))

;;; dict.el,v1.4 ends here
