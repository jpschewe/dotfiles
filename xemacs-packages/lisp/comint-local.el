;; Some comint additions.

;; The first defadvice sets comint-process-echoes for a local or remote
;; shell, that is whatever uses comint-send-input.  If the first input is
;; echoed, then it sets `comint-process-echoes' to t.  It only checks for
;; an echo the first time you run the routine.
;;
;; The second one lets you type "clear" to get the effect of a system window
;; clear command.

(defvar comint-been-in-echoes-routine nil)
(make-variable-buffer-local 'comint-been-in-echoes-routine)

(defvar comint-dont-test-echo-regexp "password\\|login"
  "*Don't run the echo check defadvice if the characters on the line match this.
Default is not to run it for password or login prompts.")
  
(defadvice comint-send-input (after check-for-echoes activate)
  "Set `comint-process-echoes' if needed to stop echoes in a shell.
Don't run this check if characters on the line in front of the cursor contain
the regexp `comint-dont-test-echo-regexp', which see."
  (if (and (not comint-been-in-echoes-routine)
	   (not (string-match comint-dont-test-echo-regexp
			      (buffer-substring
			       (save-excursion
				 (beginning-of-line) (point))
			       (point)))))
      (let ((laststring (buffer-substring-no-properties
			 (if (boundp 'comint-last-input-start)
			     (symbol-value 'comint-last-input-start))
			 (if (boundp 'comint-last-input-end)
			     (symbol-value 'comint-last-input-end)))))
	(sit-for .1)
	(save-excursion
	  (if (search-backward (concat laststring laststring) nil t)
	      (progn
		(setq comint-process-echoes t)
		(if (string-match "telnet" (buffer-name))
		    (setq telnet-remote-echoes t))
		(forward-line 1)
		(kill-line 1)))
	  (setq comint-been-in-echoes-routine t)))))

(defadvice comint-send-input (around allow-clear-command activate)
  (if (looking-back "clear")
      (progn
	(kill-word -1)
	(forward-line 0)
	(delete-region 1 (point))
	(end-of-buffer))
    ad-do-it))

(defun looking-back (regexp)
  "Return t if text before point matches regular expression REGEXP.
This function modifies the match data that `match-beginning',
`match-end' and `match-data' access; save and restore the match
data if you want to preserve them."
  (save-excursion
    (let ((beg (point)))
      (if (re-search-backward regexp nil t)
	  (if (= (match-end 0) beg)
	      t
	    nil)
	nil))))

(provide 'comint-local)
