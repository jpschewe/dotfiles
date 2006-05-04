;;; efc.el -- Emacs Foundation Classes
;; $Revision: 1.1 $ $Date: 2005-02-16 14:24:59 -0600 (Wed, 16 Feb 2005) $ 

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Kinnucan
;; Keywords: lisp, tools, classes

;; Copyright (C) 2001, 2002, 2003, 2004 Paul Kinnucan.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, US
;;; Commentary:

;; This package contains a set of eieio-based foundation classes
;; for Emacs.

;; Please send bug reports and enhancement suggestions
;; to Paul Kinnucan at <paulk@mathworks.com>

;; See end of this file for change history.

;;; Code:

(require 'eieio)
(require 'wid-edit)

(defvar efc-query-options-function nil
  "If non-nil the function to use for interactively querying options.
If nil then the default efc custom-based dialogs will be used.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;; 
;; Dialog Class                                                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass efc-dialog ()
  ((title     :initarg :title
	      :type string
	      :initform "Dialog"
	      :documentation
	      "Title of dialog")
   (buf       :initarg :buf
	      :type buffer
	      :documentation
	      "Dialog buffer")
   (initbuf   :initarg :initbuf
	      :type buffer
	      :documentation
	      "Buffer from which dialog was called.")
   )
  "Super class of EFC dialogs."
  )

(defmethod initialize-instance ((this efc-dialog) &rest fields)
  "Constructor for dialog."
  ;; Call parent initializer.
  (call-next-method))


(defmethod efc-dialog-create ((this efc-dialog)))

(defmethod efc-dialog-ok ((this efc-dialog))
  "Invoked when the user clicks the dialog's okay button. The
default method kills the dialog buffer."
  (kill-buffer (current-buffer)))

(defmethod efc-dialog-cancel ((this efc-dialog))
  "Invoked when the user clicks the dialog's Cancel button. The
default method kills the dialog buffer."
  (delete-window)
  (set-buffer (oref this initbuf))
  (pop-to-buffer (oref this initbuf))
  (kill-buffer (oref this buf)))

(defmethod efc-dialog-show ((this efc-dialog))
  (oset this initbuf (current-buffer))

  (oset this buf (get-buffer-create (oref this title)))
  (set-buffer (oref this buf))

  (efc-dialog-create this)

  (widget-put
   (widget-create 
    'push-button
    :notify 
    (lambda (button &rest ignore) (efc-dialog-ok (widget-get button :dialog)))
    "Ok")
   :dialog this)

  (widget-insert "  ")

  (widget-put
   (widget-create 
    'push-button
    :notify (lambda (button &rest ignore) (efc-dialog-cancel (widget-get button :dialog)))
    "Cancel")
   :dialog this)

   (use-local-map widget-keymap)
   (widget-setup)

  ;; Position cursor over OK button.
  ;; (forward-line 0)

  (goto-char (point-min))

  (pop-to-buffer (oref this buf)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;; 
;; Option Dialog                                                              ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass efc-option-dialog (efc-dialog)
  ((options        :initarg :options
		   :documentation
		   "Options from from which to choose.")		  
   (radio-buttons  :initarg :radio-buttons
		   :documentation
		   "Buttons for selecting options.")
   (text           :initarg :text
		   :type string
		   :initform "Select option."
		   :documentation
		   "Text to be inserted at top of dialog.")
   (selection      :initarg :selection
		   :documentation
		   "Option chosen by the user."))
   "This dialog allows a user to choose one of a set of OPTIONS by clicking
a radio button next to the option. The dialog sets SELECTION to the option
chosen by the user when the user selects the OK button on the dialog. This
dialog uses recursive edit to emulate a modal dialog.")

(defmethod initialize-instance ((this efc-option-dialog) &rest fields)
  "Dialog constructor."
  (call-next-method))

(defmethod efc-dialog-create ((this efc-option-dialog))
  (widget-insert (oref this text))
  (widget-insert "\n\n")
  (oset this radio-buttons
 	(widget-create
 	 (list
	  'radio-button-choice
	  :value (car (oref this options))
	  :args (mapcar 
		 (lambda (x) 
		   (list 'item x)) 
		 (oref this options)))))
  (widget-insert "\n"))

(defmethod efc-dialog-show ((this efc-option-dialog))
  "Shows the options dialog buffer. After showing the dialog buffer,
this method invokes recursive-edit to emulate the behavior of a modal
dialog. This suspends the current command until the user has selected
an option or canceled the dialog. See `efc-dialog-ok' and
`efc-dialog-cancel' for more information."
  (save-window-excursion
    (call-next-method)
    (recursive-edit)))


(defmethod efc-dialog-ok ((this efc-option-dialog))
  "Invoked when the user selects the OK button on the options
dialog. Sets the :selection field of THIS to the option chosen by the
user, kills the dialog buffer, and exits recursive-edit mode."
  (oset this 
	selection 
	(widget-value (oref this radio-buttons)))
  (delete-window)
  (set-buffer (oref this initbuf))
  (pop-to-buffer (oref this initbuf))
  (kill-buffer (oref this buf))
  (exit-recursive-edit))

(defmethod efc-dialog-cancel ((this efc-option-dialog))
  "Invoked when the user clicks the dialog's Cancel button.  Invokes
the default cancel method, sets the :selection field of THIS to nil,
and then exits recursive edit mode."
  (call-next-method)
  (oset this selection nil)
  (exit-recursive-edit))

(defun efc-query-options (options &optional prompt title)
  "Ask user to choose among a set of options."
  (if efc-query-options-function
      (funcall efc-query-options-function options prompt title)
    (let ((dialog
	   (efc-option-dialog
	    (or title "option dialog")
	    :text (or prompt "Select option:")
	    :options options)))
      (efc-dialog-show dialog)
      (oref dialog selection))))

;; The following code is a patch that implements Richard Stallman's fix
;; for the following error that occurs only in Emacs 21.1.1.
;;
;; Debugger entered--Lisp error: (wrong-type-argument window-live-p #<window 66>)
;;      select-window(#<window 66>)
;;      exit-recursive-edit()
;; This replacement macro fixes the problem with exit-recursive-edit on Emacs 21.
;; You'll have to recompile wid-edit.el with it.
;; (defmacro save-selected-window (&rest body)
;;   "Execute BODY, then select the window that was selected before BODY.
;; However, if that window has become dead, don't get an error,
;; just refrain from switching to it."
;;   `(let ((save-selected-window-window (selected-window)))
;;      (unwind-protect
;; 	 (progn ,@body)
;;        (if (window-live-p save-selected-window-window)
;; 	   (select-window save-selected-window-window)))))


(if (and (not (featurep 'xemacs))
	 (or
	  (string-match "21\\.1" (emacs-version))
	  (string-match "21\\.2" (emacs-version))))
    (progn
      ;; Need to load wid-edit first to ensure that
      ;; it does not get loaded after this patch and
      ;; hence override the patch.
      (require 'wid-edit)

      ;; Patched version of save-selected-window.
      (defmacro save-selected-window (&rest body)
	"Execute BODY, then select the window that was selected before BODY.
However, if that window has become dead, don't get an error,
just refrain from switching to it."
	`(let ((save-selected-window-window (selected-window)))
	   (unwind-protect
	       (progn ,@body)
	     (if (window-live-p save-selected-window-window)
		 (select-window save-selected-window-window)))))

      ;; Redefine widget-button-click to use the patched 
      ;; version of save-selected-window
      (defun widget-button-click (event)
	"Invoke the button that the mouse is pointing at."
	(interactive "@e")
	(if (widget-event-point event)
	    (let* ((pos (widget-event-point event))
		   (button (get-char-property pos 'button)))
	      (if button
		  ;; Mouse click on a widget button.  Do the following
		;; in a save-excursion so that the click on the button
		  ;; doesn't change point.
		  (save-selected-window
		    (save-excursion
		      (mouse-set-point event)
		      (let* ((overlay (widget-get button :button-overlay))
			     (face (overlay-get overlay 'face))
			     (mouse-face (overlay-get overlay 'mouse-face)))
			(unwind-protect
		       ;; Read events, including mouse-movement events
		      ;; until we receive a release event.  Highlight/
		     ;; unhighlight the button the mouse was initially
			    ;; on when we move over it.
			    (let ((track-mouse t))
			      (save-excursion
				(when face ; avoid changing around image
				  (overlay-put overlay
					       'face widget-button-pressed-face)
				  (overlay-put overlay
					       'mouse-face widget-button-pressed-face))
				(unless (widget-apply button :mouse-down-action event)
				  (while (not (widget-button-release-event-p event))
				    (setq event (read-event)
					  pos (widget-event-point event))
				    (if (and pos
					     (eq (get-char-property pos 'button)
						 button))
					(when face
					  (overlay-put overlay
						       'face
						       widget-button-pressed-face)
					  (overlay-put overlay
						       'mouse-face
						       widget-button-pressed-face))
				      (overlay-put overlay 'face face)
				      (overlay-put overlay 'mouse-face mouse-face))))

			;; When mouse is released over the button, run
				;; its action function.
				(when (and pos
					   (eq (get-char-property pos 'button) button))
				  (widget-apply-action button event))))
			  (overlay-put overlay 'face face)
			  (overlay-put overlay 'mouse-face mouse-face))))

		    (unless (pos-visible-in-window-p (widget-event-point event))
		      (mouse-set-point event)
		      (beginning-of-line)
		      (recenter)))

		(let ((up t) command)
	       ;; Mouse click not on a widget button.  Find the global
		;; command to run, and check whether it is bound to an
		  ;; up event.
		  (mouse-set-point event)
		  (if (memq (event-basic-type event) '(mouse-1 down-mouse-1))
		      (cond ((setq command ;down event
				   (lookup-key widget-global-map [down-mouse-1]))
			     (setq up nil))
			    ((setq command ;up event
				   (lookup-key widget-global-map [mouse-1]))))
		    (cond ((setq command ;down event
				 (lookup-key widget-global-map [down-mouse-2]))
			   (setq up nil))
			  ((setq command ;up event
				 (lookup-key widget-global-map [mouse-2])))))
		  (when up
		    ;; Don't execute up events twice.
		    (while (not (widget-button-release-event-p event))
		      (setq event (read-event))))
		  (when command
		    (call-interactively command)))))
	  (message "You clicked somewhere weird.")))
      ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;; 
;; Multiple Option Dialog                                                     ;;
;;                                                                            ;;
;; Contributed by Philip Lord.                                                ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass efc-multi-option-dialog (efc-option-dialog)
  ((build-message :initarg :text
                  :type string
                  :initform "Building Dialog"
                  :documentation
                  "Warning message while building dialog, as this can be slow"))
  "Provides a dialog with several sets of OPTIONS.
The dialog sets SELECTION to the options selected by the user.")

(defmethod initialize-instance ((this efc-multi-option-dialog) &rest fields)
  "Dialog constructor."
  (call-next-method))

(defmethod efc-dialog-create ((this efc-multi-option-dialog))
  (message "%s..." (oref this build-message))
  (widget-insert (oref this text))
  (widget-insert "\n\n")
  ;; use radio buttons slot as list of radio buttons rather than.
  (oset this radio-buttons
 	(mapcar
         (lambda(list)
           (prog1
               (widget-create
                (list
                 'radio-button-choice
                 :value
                 (efc-multi-option-dialog-default this list)
                 :args (mapcar
                        (lambda (x)
                          (list 'item x))
                        list)))
             (widget-insert "\n")))
         (efc-multi-option-dialog-sort this
                                       (oref this options))))
  (widget-insert "\n")
  (message "%s...done" (oref this text)))

(defmethod efc-dialog-ok((this efc-multi-option-dialog))
  ;; set the selection up as a list rather a simple result
  (oset this selection
        (mapcar
         (lambda(widget)
           (widget-value widget))
         (oref this radio-buttons)))
  (delete-window)
  (set-buffer (oref this initbuf))
  (pop-to-buffer (oref this initbuf))
  (kill-buffer (oref this buf))
  (exit-recursive-edit))


(defmethod efc-multi-option-dialog-default ((this efc-multi-option-dialog) list)
  "Pick the default from a collection of options."
  (if (= 1 (length list))
      (car list)))

(defmethod efc-multi-option-dialog-sort ((this efc-multi-option-dialog) list)
  "Sort the options."
  ;; sort the ones with the most options first...
  (sort list
        (lambda(a b)
          (> (length a)
             (length b)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;; 
;; Compiler Class                                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass efc-compiler ()
  ((name             :initarg :name
		     :type string
		     :documentation "Compiler name.")
   (buffer           :initarg :buffer
	             :type buffer
	             :documentation
	             "Compilation buffer")
   (window           :initarg :window
		     :type window
		     :documentation
		     "Window that displays the compilation buffer.")
   (exec-path        :initarg :exec-path
		     :type string
		     :documentation "Path of compiler executable.")
   (comp-finish-fcn  :initarg :comp-finish-fcn
		     :type function
		     :documentation "Function to invoke at end of compilation."))
  "Class of compiler-like applications.")

(defmethod create-buffer ((this efc-compiler))
  "Create a buffer to display the output of a compiler process."
  (save-excursion
    (let ((buf (get-buffer-create (format "*%s*" (oref this name))))
	  (error-regexp-alist compilation-error-regexp-alist)
	  (enter-regexp-alist (if (boundp 'compilation-enter-directory-regexp-alist) 
                                  compilation-enter-directory-regexp-alist))
	  (leave-regexp-alist (if (boundp 'compilation-leave-directory-regexp-alist)
                                  compilation-leave-directory-regexp-alist))
	  (file-regexp-alist (if (boundp 'compilation-file-regexp-alist)
                                 compilation-file-regexp-alist))
	  (nomessage-regexp-alist (if (not jde-xemacsp) compilation-nomessage-regexp-alist))
	  (parser compilation-parse-errors-function)
	  (error-message "No further errors")
	  (thisdir default-directory))

      (oset this :buffer buf)

      (set-buffer buf)

      ;; Make sure a compiler process is not
      ;; already running.
      (let ((compiler-proc (get-buffer-process (current-buffer))))
	(if compiler-proc
	    (if (or (not (eq (process-status compiler-proc) 'run))
		    (yes-or-no-p
			 (format "A %s process is running; kill it?" (oref this name))))
		(condition-case ()
		    (progn
		      (interrupt-process compiler-proc)
		      (sit-for 1)
		      (delete-process compiler-proc))
		  (error nil))
	      (error "Cannot have two processes in `%s' at once"
			 (buffer-name)))))

      ;; In case the compiler buffer is current, make sure we get the global
      ;; values of compilation-error-regexp-alist, etc.
      (kill-all-local-variables)

      ;; Clear out the compilation buffer and make it writable.
      (setq buffer-read-only nil)
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (buffer-enable-undo (current-buffer))

      (compilation-mode (oref this name))

      (set (make-local-variable 'compilation-parse-errors-function) parser)
      (set (make-local-variable 'compilation-error-message) error-message)
      (set (make-local-variable 'compilation-error-regexp-alist)
	     error-regexp-alist)
      (if (not jde-xemacsp)
	  (progn
	    (set (make-local-variable 'compilation-enter-directory-regexp-alist)
		 enter-regexp-alist)
	    (set (make-local-variable 'compilation-leave-directory-regexp-alist)
		 leave-regexp-alist)
	    (set (make-local-variable 'compilation-file-regexp-alist)
		 file-regexp-alist)
	    (set (make-local-variable 'compilation-nomessage-regexp-alist)
	      nomessage-regexp-alist)))

      (if (slot-boundp this 'comp-finish-fcn)
	  (set (make-local-variable 'compilation-finish-function)
	       (oref this comp-finish-fcn)))

      (setq default-directory thisdir
	    compilation-directory-stack (list default-directory)))))

(defmethod get-args ((this efc-compiler))
  "Get a list of command-line arguments to pass to the
compiler process.")


(defmethod exec ((this efc-compiler))
  "Start the compiler process."

  (create-buffer this)

  ;; Pop to checker buffer.
  (let ((outwin (display-buffer (oref this :buffer))))
    (compilation-set-window-height outwin)
    (oset this :window outwin))

  (if (not (featurep 'xemacs))
      (if compilation-process-setup-function
	  (funcall compilation-process-setup-function)))     

  (let* ((outbuf (oref this :buffer))
	 (executable-path (oref this exec-path))
	 (args (get-args this)))

    (save-excursion
      (set-buffer outbuf)

      (insert (format "cd %s\n" default-directory))

      (insert (concat
	       executable-path
	       " "
               (mapconcat 'identity args " ")
	       "\n\n"))

      (let* ((process-environment (cons "EMACS=t" process-environment))
	     (w32-quote-process-args ?\")
	     (win32-quote-process-args ?\") ;; XEmacs
	     (proc (apply 'start-process 
			  (downcase mode-name)
			  outbuf
			  executable-path
			  args)))
	(set-process-sentinel proc 'compilation-sentinel)
	(set-process-filter proc 'compilation-filter)
	(set-marker (process-mark proc) (point) outbuf)
	(setq compilation-in-progress
	      (cons proc compilation-in-progress)))

      (set-buffer-modified-p nil)
      (setq compilation-last-buffer (oref this :buffer)))))




(provide 'efc)


;; Change History
;; $Log: efc.el,v $
;; Revision 1.1  2005/02/16 20:24:58  jpschewe
;; New JDEE.
;;
;; Revision 1.16  2004/12/10 03:38:04  paulk
;; Fix efc-compiler to make and set a buffer-local version of compilation-finish-function.
;; Thanks To David Evers.
;;
;; Revision 1.15  2004/07/01 14:04:39  jslopez
;; Compatibility fix for emacs in CVS. Replaces jde-xemacsp check for boundp for
;; the following variables: compilation-nomessage-regexp-alist,
;; compilation-file-regexp-alist, compilation-leave-directory-regexp-alist,
;; compilation-enter-directory-regexp-alist. Uses the compilation-mode without a
;; parameter. The emacs in CVS does not contain the variables, or the parameter
;; for compilation mode.
;;
;; Revision 1.14  2004/03/16 07:42:09  paulk
;; Define new efc-multi-option-dialog. Thanks to Philip Lord.
;;
;; Revision 1.13  2003/11/29 05:50:18  paulk
;; The efc-dialog-show method ofr efc-option-dialog now uses save-window-excursion
;; to restore the user's original window conversion after showing the selection
;; buffer.
;;
;; Revision 1.12  2003/08/25 04:57:30  paulk
;; Adds efc-compiler class. This class implements an Emacs interface to an external process
;; that generates compiler-like output.
;;
;; Revision 1.11  2003/06/07 04:04:10  paulk
;; Fix regexp for matching Emacs versions. Thanks to David Ponce.
;;
;; Revision 1.10  2003/03/28 05:33:29  andyp
;; XEmacs optimizations for JDEbug and efc.
;;
;; Revision 1.9  2002/03/29 12:40:27  paulk
;; Adds efc-query-option function.
;;
;; Revision 1.8  2002/03/19 12:24:47  paulk
;; Updated live-window error patch to work for Emacs 21.2.
;;
;; Revision 1.7  2002/02/21 05:35:39  paulk
;; efc-dialog class now creates the dialog buffer in
;; the efc-dialog-show method instead of in the
;; intialize-instance method. This permits reuse
;; of the dialog buffer object and hence persistance
;; of user settings in the dialog.
;;
;; Revision 1.6  2002/01/25 10:41:55  paulk
;; Fixes Lisp error: (wrong-type-argument window-live-p #<window 66>) that
;; occurs in Emacs 21.1.1 when the user clicks an efc dialog box button.
;;
;; Revision 1.5  2002/01/06 06:54:06  paulk
;; Finally found a fix for the efc dialog class that works around
;; the delete-other-windows bug in Emacs 21.
;;
;; Revision 1.4  2001/12/04 14:45:55  jslopez
;; Change jde-xemacs for (featurep 'xemacs).
;;
;; Revision 1.3  2001/12/04 12:32:34  jslopez
;; Fixes typo (efc-xemacsp to jde-xemacsp).
;;
;; Revision 1.2  2001/12/04 06:06:34  paulk
;; Remove carriage returns.
;;
;; Revision 1.1  2001/12/04 05:23:20  paulk
;; Initial revision.
;;
;;

;; End of efc.el
