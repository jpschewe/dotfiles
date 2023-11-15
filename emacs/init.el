;; -*- Mode: Emacs-Lisp -*-

;; MELPA package support
(require 'package)

;; use install-packages.sh to install packages

;; If there are no archived package contents, refresh them
;;(when (not package-archive-contents)
;;  (package-refresh-contents))

; rebuild installed packages - useful when changing versions of emacs
;; (byte-recompile-directory package-user-dir nil 'force)

;; use package-install-selected-packages to install the packages in the variable package-selected-packages

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/"))))


  (when (< emacs-major-version 28)
    (add-to-list 'package-archives
                 '("nongnu" . "https://elpa.nongnu.org/nongnu/")))
  )
;should not be needed (package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aquamacs-additional-fontsets nil t)
 '(aquamacs-customization-version-id 216 t)
 '(aquamacs-tool-bar-user-customization nil t)
 '(auth-source-save-behavior nil)
 '(default-frame-alist
    '((menu-bar-lines . 1)
      (foreground-color . "Black")
      (background-color . "White")
      (cursor-type . box)
      (cursor-color . "Red")
      (internal-border-width . 0)
      (left-fringe . 1)
      (right-fringe)
      (fringe)))
 '(display-time-mode t)
 '(gutter-buffers-tab-enabled nil)
 '(gutter-buffers-tab-visible nil)
 '(indent-tabs-mode nil)
 '(ns-alternate-modifier 'alt)
 '(ns-tool-bar-display-mode nil t)
 '(ns-tool-bar-size-mode nil t)
 '(package-get-remote '(("ftp.xemacs.org" "/pub/xemacs/packages")))
 '(package-selected-packages
   '(trashed x509-mode forge eat journalctl-mode bash-completion compat ssh magit tramp groovy-mode ascii-table php-mode pandoc pandoc-mode lsp-mode rustic cargo osx-clipboard markdown-mode diminish csharp-mode applescript-mode elpy go-mode yaml-mode))
 '(query-user-mail-address nil)
 '(safe-local-variable-values
   '((whitespace-newline . t)
     (whitespace-style face trailing lines-tail space-before-tab indentation empty)))
 '(semanticdb-default-save-directory (concat "/tmp/" user-login-name "/xemacs-cache"))
 '(send-mail-function 'mailclient-send-it)
 '(visual-line-mode nil t))


;; check which emacs is running
(defvar running-xemacs (featurep 'xemacs))
;;(defvar running-gnuemacs (string-match "^GNU Emacs" (emacs-version)))
(defvar running-gnuemacs (featurep 'emacs))
(defvar running-aquamacs	(string-match "Aquamacs" (emacs-version)))
(defvar running-carbon	(and (string-match "Carbon" (emacs-version))
                             (not ver-p-aquamacs)))
(defvar running-unix (or (eq system-type 'linux)
			 (eq system-type 'gnu/linux)
			 (eq system-type 'usg-unix-v)
			 (eq system-type 'berkeley-unix)))

;; only warn me of errors
(setq display-warning-minimum-level 'error)
(setq log-warning-minimum-level 'error)

;; take care of some custom variables right up front


(when (and (not running-xemacs) (eq system-type 'darwin))
  (setq mac-command-modifier 'meta) ;; Sets the command (Apple) key as Meta
  (setq mac-option-modifier 'alt)   ;; Sets the option (Apple) key as alt
  (cond (running-aquamacs
	 (global-set-key "\M-`" 'raise-next-frame))
	(running-gnuemacs
	 (global-set-key "\M-`" 'other-frame))
	))

;; allow some variables to be loaded automatically
(when running-aquamacs
  (custom-set-variables
   '(tabbar-mode nil nil (tabbar))
   )      
  (setq safe-local-variable-values (quote ((Syntax . COMMON-LISP) (Base . 10))))
  )

;; setup paths
(when (not running-xemacs)
  (let ((base-dir (expand-file-name "~/.xemacs/xemacs-packages/lisp")))
    (add-to-list 'load-path base-dir)
    (dolist (fileOrDir (directory-files base-dir))
      (if (not (string= "." (substring fileOrDir 0 1)))
	  (let ((path (expand-file-name fileOrDir base-dir)))
	    (if (file-directory-p path)
		(add-to-list 'load-path path)
	      ))))))

;;set faces up front
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(when (not (boundp 'windows-nt)) (setq windows-nt nil))

;;(setq debug-on-error t)
;;(setq debug-on-error nil)

;;;;;;;;;;;
;;
;; Basic settings
;;
;;;;;;;;;;;;
(message "Basic settings")
;; toolbar stuff
(message "toolbar")
(cond (running-xemacs
       (set-default-toolbar-position 'top)
       (set-specifier default-toolbar-visible-p nil))
      (running-aquamacs
       ;; remove toolbar
       (tool-bar-mode -1)
       (setq aquamacs-set-creator-codes-after-writing-files nil)
       )
      (running-gnuemacs
       (tool-bar-mode -1)
       (set-scroll-bar-mode 'right)
       )
      )


(setq next-line-add-newlines nil;; no newlines at EOF
      mouse-yank-at-point t;; yank from current position, ignore mouse
      passwd-invert-frame-when-keyboard-grabbed nil;; don't invert on passwd
      inhibit-startup-message t;; no startup screen
      default-fill-column 75;; set width for auto fill
      default-major-mode 'text-mode;; set default mode
      kept-new-versions 1
      kept-old-versions 1
      delete-old-versions t
      version-control t
      ;;completion-ignore-case t          ;case insensitive file matching
      find-file-compare-truenames t;; watch out for symlinks
      vc-follow-symlinks t ;; follow symlinks without question
      ;;find-file-use-truenames t ;; always find the real filename
      ;;Manual-program "man"
      visible-bell t;; don't beep
      scroll-step 5			; set how many lines to scroll at a time
      enable-local-eval t ;;don't propmt me about evals in files
      delete-by-moving-to-trash t ;; don't delete files, move them to the trash
      )

;; turn off file locking
(setq inhibit-clash-detection t)

;; web browser integration
;;(cond (running-unix
;;       (setq browse-url-browser-function 'browse-url-kfm))
;;       (t
;;	(setq browse-url-browser-function 'browse-url-mozila)))
(cond ((or (eq system-type 'windows-nt) 
	   (eq system-type 'cygwin32))
       (setq browse-url-browser-function 'browse-url-firefox))
      (running-unix
       (setq browse-url-generic-program "xdg-open")
       (setq browse-url-browser-function 'browse-url-generic)
       )
      ((eq system-type 'darwin)
       (setq browse-url-generic-program "open")
       (setq browse-url-browser-function 'browse-url-generic)
       ))



;; set the title to make it easy to determine which XEmacs is running
;; (let ((host (downcase (system-name))))
;;   (setq frame-title-format (concat "Emacs: " (user-real-login-name) "@" (car (split-string host "\\.")) ": %b")))
(setq frame-title-format '("" invocation-name "@" system-name "\t" "%b"))

;; Change all yes/no prompts to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; turn on line numbers
(line-number-mode t)
;; turn on column numbers
(column-number-mode t)

;; turn off dialog boxes
(setq use-dialog-box nil)

;; move the mouse if it gets in the way
;;(mouse-avoidance-mode 'exile)

;;;;;;;;;;;
;;
;; System specific
;;
;;;;;;;;;;;;
(message "System specific")
(cond ((or (eq system-type 'windows-nt) 
	   (eq system-type 'cygwin32))
       ;;XEmacs on NT works better this way (I hope)
       (setq directory-sep-char ?/)

       (setq openoffice-executable "c:/packages/OpenOffice/program/soffice.exe")

       (eval-after-load "compile"
	 '(progn
	    ;; make xemacs handle the spaces in filenames
	    (add-to-list 'compilation-error-regexp-alist '("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\):[ \t]*\\([0-9]+\\)[: \t]" 1 3))
	    ))

       
       ;;----------------------------------------------------------------------------
       ;; Cygnus bash as subshell
       ;;
       ;; Prerequisit: Set the SHELL environment variable to bash before
       ;; starting emacs.  The variable shell-file-name is initialized to the
       ;; enviroment variable when emacs starts up.
       (if (eq system-type 'windows-nt);; seems to mess up the cygwin version of xemacs
	   (cond
	    ((file-exists-p (expand-file-name "c:\\packages\\cygwin\\bin\\bash.exe"))
	     (setq shell-file-name "c:\\packages\\cygwin\\bin\\bash.exe"))
	    ((file-exists-p (expand-file-name "c:\\cygwin\\bin\\bash.exe"))
	     (setq shell-file-name "c:\\cygwin\\bin\\bash.exe"))))
       
       ;; Use -i to force .bashrc file to be run, otherwise aliases defined
       ;; in you .bashrc file will not be available.
       (setq shell-command-switch "-c")
       
       ;; nasty stuff to get cygwin 1.3.1 to work right
       (setq mswindows-construct-process-command-line-alist
	     '(("[\\/].?.?sh\\." . mswindows-construct-vc-runtime-command-line)
	       ("[\\/]command\\.com$" . mswindows-construct-command-command-line)
	       ("[\\/]cmd\\.exe$" . mswindows-construct-command-command-line)
	       ("" . mswindows-construct-vc-runtime-command-line)))
       ;;Set this to true when debugging processes
       ;;(setq debug-mswindows-process-command-lines nil)
       )
      (running-unix
       (cond
	((file-exists-p (expand-file-name "/usr/bin/ooffice"))
	 (setq openoffice-executable (expand-file-name "/usr/bin/ooffice")))
	((file-exists-p (expand-file-name "~/.ooo-1.1/soffice"))
	 (setq openoffice-executable (expand-file-name "~/.ooo-1.1/soffice")))
	((file-exists-p "/opt/OpenOffice.org/program/soffice")
	 (setq openoffice-executable "/opt/OpenOffice.org/program/soffice"))
	((file-exists-p "/usr/lib/ooo-1.1/program/soffice")
	 (setq openoffice-executable "/usr/lib/ooo-1.1/program/soffice"))
	(t
	 (setq openoffice-exeutable "openoffice-not-found")))
       )
      ((eq system-type 'darwin)
       (cond
	((file-exists-p "/Applications/OpenOffice.org.app")
	 (setq openoffice-executable "open /Applications/OpenOffice.org.app"))
	(t
	 (setq openoffice-exeutable "openoffice-not-found"))
	)

       ;; only use trash command on Darwin if the executable is found
       (if (executable-find "trash")
	   ;; true
	   (progn
	    (defun system-move-file-to-trash (file)
	   "Use \"trash\" to move FILE to the system trash."
	   (call-process (executable-find "trash")
			 nil 0 nil
			 file)))
	 ;; false
	 (setq delete-by-moving-to-trash nil)
	 )
       
       ;; for macports
       ;(let ((path-entries (split-string (getenv "PATH") ":")))
	 ;(add-to-list 'path-entries "/opt/local/bin")
	 ;(add-to-list 'path-entries "/Users/jschewe/bin")
	 ;(setenv "PATH" (mapconcat 'identity path-entries ":"))
;	 )

       ))


;;;;;;;;;;;
;;
;; Clipboard
;;
;;;;;;;;;;;;
(cond
 (running-gnuemacs
  (setq x-select-enable-clipboard t))
 (running-xemacs
  (setq interprogram-cut-function 'own-clipboard)
  (setq interprogram-paste-function 'get-clipboard))
 (running-aquamacs
  (require 'osx-clipboard)))


;;;;;;;;;;;
;;
;; Keybindings
;;
;;;;;;;;;;;;
(message "Keybindings")

;; (when (or running-gnuemacs running-aquamacs)
;;   (global-set-key [(control tab)] 'other-window)
;;   )

(defvar prefix-key-jps "\C-co" "Used as a prefix for my keybindings")

(global-set-key (concat prefix-key-jps "f") 'iconify-frame)
(global-set-key (concat prefix-key-jps "d") 'delete-region)
(global-set-key (concat prefix-key-jps "s") 'shell-jps)
(global-set-key (concat prefix-key-jps "m") 'new-eat-shell-jps)
(global-set-key (concat prefix-key-jps "e") 'eshell-jps)

(defun shell-jps ()
  (interactive)
  ;;(let ((default-directory (if (file-remote-p default-directory) "~" default-directory)))
  ;;  (call-interactively 'shell)))
  ;;(call-interactively 'shell)
  (call-interactively 'eat)
  )

(defun new-shell-jps ()
    "Create a new shell buffer"
  (interactive)
  (let ((current-prefix-arg '-)) ;; emulate C-u
    (call-interactively 'shell-jps)))

(defun new-eat-shell-jps ()
  (interactive)
  (let ((current-prefix-arg '-))
    (let ((eat-buffer (call-interactively 'eat)))
      (let ((new-name (read-from-minibuffer "Name: " (buffer-name eat-buffer))))
        (switch-to-buffer eat-buffer)
        (rename-buffer new-name t)))))

(defun eshell-jps ()
  (interactive)
  (call-interactively 'eshell))

;;(global-set-key (concat prefix-key-jps "g") 'gnus)
(global-set-key (concat prefix-key-jps "n") 'rename-buffer)
(global-set-key (concat prefix-key-jps "r") 'revert-buffer-jps)
(global-set-key (concat prefix-key-jps "b") 'bury-buffer)


(global-set-key (concat prefix-key-jps "a") 'tags-search-jps)
(defun tags-search-jps (regex)
  "tags-search and default to current-word"
  (interactive (list (read-string "Tags search (regexp): " (current-word))))
  (tags-search regex)
  )

(defun insert-date-signature ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d") " -- " (user-full-name)))
(global-set-key (concat prefix-key-jps "c") 'insert-date-signature)
 
(global-set-key [(meta return)] 'hippie-expand);; expand
;; replaced by C-x C-q (global-set-key [insert] 'toggle-read-only)

;;make my scroll wheel work
(global-set-key [(button4)] 'scroll-down-command)
(global-set-key [(button5)] 'scroll-up-command)

;(global-set-key [(control ?s)] 'isearch-forward-regexp)
;(global-set-key [(control ?r)] 'isearch-backward-regexp)

;;(global-set-key [f6] 'x-copy-primary-selection)
;;(global-set-key [f8] 'x-yank-clipboard-selection)
;;(global-set-key [f9] 'function-menu)
;;(global-set-key [f10] 'x-kill-primary-selection)
;;(global-set-key '(shift button3) 'mouse-function-menu)

;;(global-set-key [kp-enter] "\C-m")
;;(global-set-key [kp-0] "0")
;;(global-set-key [kp-1] "1")
;;(global-set-key [kp-2] "2")
;;(global-set-key [kp-3] "3")
;;(global-set-key [kp-4] "4")
;;(global-set-key [kp-5] "5")
;;(global-set-key [kp-6] "6")
;;(global-set-key [kp-7] "7")
;;(global-set-key [kp-8] "8")
;;(global-set-key [kp-9] "9")
;;(global-set-key [kp-divide] "/")
;;(global-set-key [kp-multiply] "*")
;;(global-set-key [kp-subtract] "-")
;;(global-set-key [kp-add] "+")
;;(global-set-key [kp-decimal] ".")
;;(global-set-key [kp-end] [end])
;;(global-set-key [kp-home] [home])
;;(global-set-key [kp-prior] [prior])
;;(global-set-key [kp-next] [next])
;;(global-set-key [kp-delete] [delete])
;;(global-set-key [kp-insert] [insert])
;;(global-set-key [kp-left] [left])
;;(global-set-key [kp-right] [right])
;;(global-set-key [kp-up] [up])
;;(global-set-key [kp-down] [down])
;; for sun3-50
;;(global-set-key [f27] [home])
;;(global-set-key [f33] [end])
;;(global-set-key [f29] [prior])
;;(global-set-key [f35] [next])
;;(global-set-key [(control f27)] 'beginning-of-buffer)
;;(global-set-key [(control f33)] 'end-of-buffer)

;(global-set-key "\C-x\C-v" 'view-file)
(global-set-key "\C-m" 'newline-and-indent)

;(autoload 'top "top-mode" nil t)
;(global-set-key (concat prefix-key-jps "t") 'top)

(global-set-key "\C-xk" 'kill-this-buffer)

(global-set-key [scroll-lock] 'overwrite-mode)

(global-set-key "\C-xt" 'toggle-truncate-lines)

; I found this annoying most times as it keeps wanting to suggst files in dired that aren't what I want
;;(ffap-bindings)
;; end keybindings

;;;;;;;;;;;
;;
;; Font Lock
;;
;;;;;;;;;;;;
(message "Font lock")
;;turn off stupid extra buffer when fontifying things
(setq progress-feedback-use-echo-area t)
;(set-face-background 'default "light gray")
(setq font-lock-maximum-decoration t
      font-lock-use-colors '(color)
      font-lock-auto-fontify t
      font-lock-verbose nil;; no messages while fontifying
      lazy-lock-stealth-verbose nil;; no messages while fontifying
      lazy-lock-stealth-time nil
      query-replace-highlight t
      font-menu-ignore-scaled-fonts nil
      )

;;make sure cache directory exists
(let ((cache-dir (concat "/tmp/" user-login-name "/xemacs-cache")))
  (if running-xemacs
      (make-directory-path cache-dir)
    (make-directory cache-dir t))
  (setq fast-lock-cache-directories (list cache-dir)))


(require 'font-lock)
(when running-xemacs
  (require 'fast-lock)
  (setq font-lock-support-mode 'fast-lock-mode)
  (add-hook 'font-lock-mode-hook 'turn-on-fast-lock)
  ;;(add-hook 'font-lock-mode-hook 'turn-on-lazy-lock)
  )

;; used for selected text
(set-face-attribute 'region nil :background "#666")

(set-face-foreground 'font-lock-warning-face "yellow")
(set-face-background 'font-lock-warning-face "red")
(make-face 'font-lock-todo-face)
(if running-xemacs
    (set-face-parent 'font-lock-todo-face 'default))
(set-face-foreground 'font-lock-todo-face "red")
(defun add-special-font-lock-faces-jps (vars)
  "Add my special highlighting to each font-lock var in the given list"
  (map 'list '(lambda (font-var)
		(add-to-list font-var
			     '("\\<\\(FIX\\)" 1 font-lock-warning-face t))
		(add-to-list font-var
			     '("\\<\\(HACK\\)" 1 font-lock-warning-face t))
		(add-to-list font-var
			     '("\\<\\(TODO\\)" 1 font-lock-todo-face t))
		(add-to-list font-var
			     '("\\<\\(NOTE\\)" 1 font-lock-todo-face t))
		) vars))

;;;;;;;;;;;
;;
;; Printing
;;
;;;;;;;;;;;;
(message "Printing")
(setq
 ;; kprinter
 ;;lpr-command "kprinter"
 ;;lpr-switches '("--stdin") ;;(list "-r" "-G" "-b\"- Schewe -\"")
 ;;lpr-page-header-switches '("-F" "-l 58") ; be compatible with linux pr

 ;; enscript
 lpr-command "enscript"
 lpr-switches '(
		"-r" ; landscape
		"-G" ; fancy header
		"-b\"- %n -\"" ; include username in header
		"-DDuplex:true" ; double sided
		)
 ; lpr-add-switches t			; add -J title
 ; lpr-page-header-program "pr"

 ;; lpr for ps
 ; ps-print-header-frame nil		; save some toner
 ; ps-lpr-command "lpr"
					; ps-lpr-switches nil

;lpr-switches '(
;		"-o" "sides=two-sided-long-edge"
;		)
 )


;;;;;;;;;;;
;;
;; Parens
;;
;;;;;;;;;;;;
(message "Parens")
(if running-xemacs
    (paren-set-mode 'paren)
  (progn
    (show-paren-mode 1)
    (setq show-paren-mode t)
    (setq show-paren-style 'parenthesis)))


;;;;;;;;;;;
;;
;; Grep
;;
;;;;;;;;;;;;
(message "grep")
;;change the default to be my perl script
;;(setq grep-command "grep -n -R --exclude='*CVS*' --exclude='*.svn*' ")
(setq grep-find-command "find . \\( -name .git -prune \\) -o \\( -name .svn -prune \\) -o \\( -name CVS -prune \\) -o -type f -print0 | xargs -0 grep -n ")


;;;;;;;;;;;
;;
;; comint
;;
;;;;;;;;;;;;
(message "comint")
(require 'comint)

;;handle password prompts
;;; Enter PKCS#11 token PIN for PIV_II:
(setq comint-password-prompt-regexp
      (concat
       comint-password-prompt-regexp
       "\\|"  "^.*\\(?:[Pp]ass\\(?:word\\| ?phrase\\)\\).*:\\s-*\\'"
       "\\|" "token PIN"
       "\\|" "Enter PIN"
       "\\|" "\\(will be hidden\\)"
       )
       )

(defun comint-common-hook-jps ()
  (local-set-key [up] 'comint-previous-matching-input-from-input)
  (local-set-key [down] 'comint-next-matching-input-from-input)
  (local-set-key "\M-p" 'comint-previous-matching-input-from-input)
  (local-set-key "\M-n" 'comint-next-matching-input-from-input)
  (local-set-key "\C-cc" 'comint-continue-subjob)
  (if running-xemacs
      (turn-off-font-lock))
  ;(local-set-key [tab] 'comint-dynamic-complete)
  )

(add-hook 'gdb-mode-hook 'comint-common-hook-jps)
(add-hook 'shell-mode-hook 'comint-common-hook-jps)

;; Make "M-x shell-command" use the same shell as "M-x shell"
(setq explicit-shell-file-name shell-file-name)
       

(defun ssh-hook-jps ()
  (comint-common-hook-jps)
  (setq comint-process-echoes nil);;some do and some don't, so leave the extra copies in there
  ;;(add-to-list 'comint-output-filter-functions 'comint-strip-ctrl-m)
  (setq ssh-directory-tracking-mode t)
  (shell-dirtrack-mode t)
  (setq dirtrackp nil)
  )
(add-hook 'ssh-mode-hook 'ssh-hook-jps)

(require 'bash-completion)
(bash-completion-setup)


;; eshell setup
(defun eshell-hook-jps ()
  (add-to-list 'eshell-visual-options '("git" "--help" "--paginate"))
  (add-to-list 'eshell-visual-subcommands '("git" "log" "diff" "show" "nlog" "branch"))
  ;;(setq eshell-prefer-lisp-functions t)
  )

(add-hook 'eshell-mode-hook 'eshell-hook-jps)

(add-hook 'eshell-load-hook #'eat-eshell-mode)


;;;;;;;;;;;
;;
;; ksh-mode
;;
;;;;;;;;;;;;
(message "ksh-mode")
(defun ksh-mode-hook-jps ()
  (setq indent-tabs-mode nil)
  (font-lock-mode)
  (when running-xemacs
      (add-special-font-lock-faces-jps (list 'ksh-font-lock-keywords)))
  )
(add-hook 'ksh-mode-hook  'ksh-mode-hook-jps)

;;;;;;;;;;;
;;
;; sh-mode
;;
;;;;;;;;;;;;
(message "sh-mode")
(add-hook 'sh-mode-hook  'ksh-mode-hook-jps)
(when running-xemacs
    (add-hook 'sh-mode-hook '(lambda ()
			       (add-special-font-lock-faces-jps (list 'sh-font-lock-keywords 'sh-font-lock-keywords-1 'sh-font-lock-keywords-2))))
)

;;;;;;;;;;;
;;
;; text-mode
;;
;;;;;;;;;;;;
;; Turn on word-wrap in text modes
;;(add-hook 'text-mode-hook 'turn-on-auto-fill)
(when (or running-xemacs running-aquamacs)
  (progn
    (message "text-mode")
    (require 'filladapt)
    (add-hook 'text-mode-hook 
	      (lambda nil  
		(filladapt-mode 1) 
		))))

;;;;;;;;;;;
;;
;; Scheme
;;
;;;;;;;;;;;;
(message "Scheme")
(add-hook 'scheme-mode-hook 'scheme-mode-hook-jps)
(defun scheme-mode-hook-jps ()
  (add-special-font-lock-faces-jps (list 'scheme-font-lock-keywords)))
(add-to-list 'auto-mode-alist '("\\.ss$" . scheme-mode))


;;;;;;;;;;;
;;
;; Alter
;;
;;;;;;;;;;;;
(add-to-list 'auto-mode-alist  '("\\.alt$" . scheme-mode))
(add-to-list 'auto-mode-alist  '("\\.lib$" . scheme-mode))

;;;;;;;;;;;
;;
;; Lisp
;;
;;;;;;;;;;;;
(message "Lisp")
(defun lisp-mode-hook-jps ()
  (add-special-font-lock-faces-jps (list 'lisp-font-lock-keywords 'lisp-font-lock-keywords-1 'lisp-font-lock-keywords-2)))
(add-hook 'lisp-mode-hook 'lisp-mode-hook-jps)

;;;;;;;;;;;
;;
;; LaTex
;;
;;;;;;;;;;;;
(message "LaTex")
(defun tex-mode-hook-jps ()
  (auto-fill-mode 1)

  ;; set the pdf viewer
  (cond (running-unix
	 (setq TeX-output-view-style (cons '("^pdf$" "." "xdg-open %o") TeX-output-view-style)))
	((eq system-type 'darwin)
	 (setq TeX-output-view-style (cons '("^pdf$" "." "open %o") TeX-output-view-style))))
  )
(add-hook 'tex-mode-hook 'tex-mode-hook-jps)
(add-hook 'LaTeX-mode-hook 'tex-mode-hook-jps)

(setq tex-run-command "pdflatex")

;; function to fill sentences to make VCS LaTeX easier
(eval-when-compile (if (functionp 'TeX-load-hack)
                       (progn (TeX-load-hack)
                              (require 'latex))))
(defun fill-sentence ()
  (interactive)
  (save-excursion
    (or (eq (point) (point-max)) (forward-char))
    (backward-sentence)
    ;; work around bug in backward-sentence (if sentence is first in
    ;; paragraph, it'll got to blank line above sentence instead of
    ;; beginning of sentence)
    (forward-word) (backward-word)
    ;(indent-according-to-mode)
    (let ((beg (point))
          (ix (string-match "LaTeX" mode-name)))
      (forward-sentence)
      (if (and (functionp 'LaTeX-fill-region-as-paragraph)
               ix (equal "LaTeX" (substring mode-name ix)))
          (LaTeX-fill-region-as-paragraph beg (point))
        (fill-region-as-paragraph beg (point))))))

;; AUCTeX uses LaTeX-mode-hook, the built-in latex-mode uses
;; latex-mode-hook
(dolist (hookvar '(latex-mode-hook LaTeX-mode-hook))
  ;; key to fill senteces in LaTeX mode
  (add-hook hookvar
            '(lambda () (global-set-key (kbd "M-q") 'fill-sentence))))


;;;;;;;;;;;
;;
;; VC mode
;;
;;;;;;;;;;;;
;; VC mode doesn't really buy me anything, just disable it
(setq vc-handled-backends nil)
;;(add-hook 'vc-load-vc-hooks
;;	  '(lambda ()
;;	     ;;(delete 'Git vc-handled-backends)
;;	     (setq vc-handled-backends nil)
;;	     ))

;;;;;;;;;;;
;;
;; HTML
;;
;;;;;;;;;;;;
(message "HTML")
(defun html-mode-hook-jps ()
  (auto-fill-mode -1)
  )
(add-hook 'html-mode-hook 'html-mode-hook-jps)

;; setup the template for new html files
(eval-after-load "html-helper-mode"
  '(progn
    (setq tempo-template-html-skeleton
	  '(
	    "<?xml version='1.0' encoding='us-ascii'?>" n>
	    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" n>
	    "<html xmlns='http://www.w3.org/1999/xhtml' lang='en' xml:lang='en'>" n>
	    "<head>" n>
	    "<meta http-equiv='Content-Type' content='text/html; charset=us-ascii'/>" n>
	    "<title>" (p "Document Title: " title) "</title>" n>
	    "<link rel='stylesheet' type='text/css' href='style.css'/>" n>
	    "</head>" n>
	    "<body>" n>
	    "<h1>" (s title) "</h1>" n>
	    p
	    >
	    ""
	    "<hr/>"n>
	    "<p>" n>
	    (html-helper-return-created-string) html-helper-timestamp-start html-helper-timestamp-end
	    n>
	    "</p>" n>
	    "</body>" n>
	    "</html>"
	    )
	  )
    t)
  )

;;;;;;;;;;;
;;
;; Dired
;;
;;;;;;;;;;;;
(message "Dired")
(setq dired-listing-switches "-alh") ; show human readable units

(defun dired-load-hook-jps ()
  ;;(define-key dired-mode-map "q" 'kill-this-buffer)
  ;;(define-key dired-mode-map "^" 'dired-jump-back)
  ;;(define-key dired-mode-map " " 'scroll-up)
  ;;(define-key dired-mode-map "b" 'scroll-down)
  ;;(define-key dired-mode-map "\C-x\C-j" 'dired-up-directory)
  
  ;;(setq dired-gnutar-program "tar")
  ;;(setq dired-unshar-program "unshar")

  ;;initialize to empty
  ;;(setq dired-auto-shell-command-alist nil)

  (when (or running-unix
	    (eq system-type 'cygwin32)
	    (eq system-type 'windows-nt)
	    (eq system-type 'darwin))
    (setq dired-listing-switches "-alh"))
  
  ;;images
  (let ((extensions '("ps" "jpg" "bmp" "pbm" "pgm" "ppm" "xbm" "xpm" "ras" "rast" "gif" "tif" "tiff" "png" "xwd")))
    ;;gimp
    (when (and running-unix running-xemacs)
      (map 'list '(lambda (ext)
		    (add-to-list 'dired-auto-shell-command-alist (list (concat "\\." ext "$") "gimp"))
		    (add-to-list 'dired-auto-shell-command-alist (list (concat "\\." ext "$") "display"))
		    (add-to-list 'dired-auto-shell-command-alist (list (concat "\\." ext "$") "gthumb")))
	   extensions)))
  
  ;;bzip
  (add-to-list 'dired-auto-shell-command-alist '("\\.bz2$" "bunzip2"))
  (add-to-list 'dired-auto-shell-command-alist '("\\.tar.bz2$" "tar -jtf"))
  (add-to-list 'dired-auto-shell-command-alist '("\\.tar.bz2$" "tar -jxvf"))

  ;;stuffit
  (when running-unix
    (add-to-list 'dired-auto-shell-command-alist '("\\.sit$" "unstuff")))
  
  ;;office documents
  (add-to-list 'dired-auto-shell-command-alist '("\\.doc$" openoffice-executable))
  (add-to-list 'dired-auto-shell-command-alist '("\\.DOC$" openoffice-executable))
  (add-to-list 'dired-auto-shell-command-alist '("\\.sxw$" openoffice-executable))
  (add-to-list 'dired-auto-shell-command-alist '("\\.xls$" openoffice-executable))
  (add-to-list 'dired-auto-shell-command-alist '("\\.sxc$" openoffice-executable))
  (add-to-list 'dired-auto-shell-command-alist '("\\.sdc$" openoffice-executable))
  (add-to-list 'dired-auto-shell-command-alist '("\\.ppt$" openoffice-executable))
  (add-to-list 'dired-auto-shell-command-alist '("\\.sxi$" openoffice-executable))
  (add-to-list 'dired-auto-shell-command-alist '("\\.rtf$" openoffice-executable))
  (add-to-list 'dired-auto-shell-command-alist '("\\.sxd$" openoffice-executable))

  ;; new formats for 2.0
  (add-to-list 'dired-auto-shell-command-alist '("\\.odt$" openoffice-executable))
  (add-to-list 'dired-auto-shell-command-alist '("\\.ott$" openoffice-executable))
  (add-to-list 'dired-auto-shell-command-alist '("\\.odm$" openoffice-executable))
  (add-to-list 'dired-auto-shell-command-alist '("\\.oth$" openoffice-executable))
  (add-to-list 'dired-auto-shell-command-alist '("\\.ods$" openoffice-executable))
  (add-to-list 'dired-auto-shell-command-alist '("\\.ots$" openoffice-executable))
  (add-to-list 'dired-auto-shell-command-alist '("\\.odg$" openoffice-executable))
  (add-to-list 'dired-auto-shell-command-alist '("\\.otg$" openoffice-executable))
  (add-to-list 'dired-auto-shell-command-alist '("\\.odp$" openoffice-executable))
  (add-to-list 'dired-auto-shell-command-alist '("\\.otp$" openoffice-executable))
  (add-to-list 'dired-auto-shell-command-alist '("\\.odf$" openoffice-executable))
  (add-to-list 'dired-auto-shell-command-alist '("\\.odb$" openoffice-executable))
  
  ;;Protege
  (add-to-list 'dired-auto-shell-command-alist '("\\.prj$" "protege"))

  ;;zip
  (when running-unix
    (add-to-list 'dired-auto-shell-command-alist '("\\.zip$" "unzip")))

  ;;adobe
  (add-to-list 'dired-auto-shell-command-alist '("\\.pdf$" "acroread"))
  (when running-unix
    (add-to-list 'dired-auto-shell-command-alist '("\\.pdf$" "kghostview"))
    (add-to-list 'dired-auto-shell-command-alist '("\\.pdf$" "gpdf"))
    (add-to-list 'dired-auto-shell-command-alist '("\\.pdf$" "xpdf"))
    )

  ;;Postscript
  (when running-unix
    (add-to-list 'dired-auto-shell-command-alist '("\\.ps$" "kghostview"))
    (add-to-list 'dired-auto-shell-command-alist '("\\.ps$" "gv"))
    )
  
  ;;dos/windows executables
  (when (or (eq system-type 'windows-nt)
	    (eq system-type 'cygwin32))
    (add-to-list 'dired-auto-shell-command-alist '("\\.exe$" "*f")))

  ;;java
  (add-to-list 'dired-auto-shell-command-alist '("\\.jar$" "jar -tvf"))
  (add-to-list 'dired-auto-shell-command-alist '("\\.jar$" "jar -xvf"))

  ;; default windows handling
;  (when (or (eq system-type 'windows-nt)
;	    (eq system-type 'cygwin32))
;    (add-to-list 'dired-auto-shell-command-alist (list ".*"
;						       (expand-file-name "winrun" (locate-data-directory "config-jps")))))

  
  (setq dired-compression-method 'gzip)
  (cond (running-xemacs
	 ;; allow one to see log files in omit mode
	 (setq dired-omit-extensions (delete ".log" dired-omit-extensions))
	 (setq dired-omit-extensions (append dired-omit-extensions '(".pyc")))
	 (setq dired-omit-extensions (append dired-omit-extensions '(".os")))))
  
  ;; don't refresh dired buffers all of the time
  (setq dired-refresh-automatically nil)
  )
;; disable for now (with-eval-after-load "dired" (dired-load-hook-jps))


;; (defun dired-mode-hook-jps ()
;;   (set 'dired-omit-files t)
;; )
;; (add-hook 'dired-mode-hook 'dired-mode-hook-jps)

;;autorevert directories
;;(defadvice dired-internal-noselect (before my-auto-revert-dired activate)
;;  (let ((buffer)(dirname (ad-get-arg 0)))
;;    (when (and (not (consp dirname))
;;               (setq buffer (dired-find-buffer-nocreate dirname nil)))
;;      (set-buffer buffer)
;;      (if (let ((attributes (file-attributes dirname))
;;                (modtime (visited-file-modtime)))
;;            (or (eq modtime 0)
;;                (not (eq (car attributes) t))
;;                (and (= (car (nth 5 attributes)) (car modtime))
;;                     (= (nth 1 (nth 5 attributes)) (cdr modtime)))))
;;          nil
;;        (kill-buffer buffer)))))



;;;;;;;;;;;
;;
;; Script-mode
;;
;;;;;;;;;;;;
;(message "script-mode")
;(defun delete-from-auto-mode-alist-jps (mode)
;  (delete-if #'(lambda (assoc)
;                 (eq (cdr assoc) mode))
;             auto-mode-alist))
;
;(delete-from-auto-mode-alist-jps 'html-mode)
;(delete-from-auto-mode-alist-jps 'sh-mode)
  
;; stuff for script.el
;;(setq interpreter-mode-alist
;;      (append
;;       ;; Note; these are strings, not regexps.
;;       ;; Use script-bash-mode not script-sh-mode for the first one
;;       ;;   if your shell is actually bash.
;;       '(( "sh" . script-sh-mode )  
;;         ( "bash" . script-bash-mode )
;;         ( "awk" . script-awk-mode )
;;         ( "gawk" . script-awk-mode )
;;         ( "nawk" . script-awk-mode )
;;         ( "tcsh" . script-csh-mode )
;;         ( "csh" . script-csh-mode ))
;;       interpreter-mode-alist))
;;(setq auto-mode-alist
;;      (append '(
;;                ("\\.awk$"  . script-awk-mode);; (replaces std awk-mode)
;;                ("\\.sh$" . script-bash-mode)
;;                )
;;              auto-mode-alist))
;;(autoload 'script-sh-mode "script" "editing mode for sh scripts" t)
;;(autoload 'script-bash-mode "script" "editing mode for bash scripts" t)
;;(autoload 'script-csh-mode "script" "editing mode for csh scripts" t)
;;(autoload 'script-awk-mode "script" "editing mode for awk scripts" t)
;;(setq script-auto-indent t)
;;
;;(defun script-mode-hook-jps ()
;;  (turn-on-lazy-lock)
;;  )
;;(add-hook 'script-mode-hook 'script-mode-hook-jps)



;;;;;;;;;;;
;;
;; c-mode
;;
;;;;;;;;;;;;
(message "c-mode")
(defun c-mode-common-hook-jps ()
  (setq indent-tabs-mode nil);; no tabs in source code

  ;; customize cc-mode
  ;;(c-set-offset 'substatement-open 
  ;;		  )
  ;;    (defvar c-hanging-braces-alist '((block-open before after)
  ;;				     (block-close before after)
  ;;				     (class-close before)
  ;;				     (inline-open before)
  ;;				     (inline-close after)
  ;;				     (substatement-open before after)))
  ;;    (defvar c-cleanup-list '(scope-operator
  ;;			     empty-defun-braces
  ;;			     defun-close-semi))
  
  ;; other customizations here
  (make-local-variable 'c-basic-offset)
  (setq c-basic-offset 2
        ;;tab-width 2
        fill-column 78)
  ;;(c-toggle-hungry-state t)
  ;;(turn-on-auto-fill)

  (define-key c-mode-map "\C-m" 'newline-and-indent)
  (define-key c-mode-map "\C-cc" 'compile)
  (define-key c-mode-map "\C-cr" 'replace-string)

  (define-key c++-mode-map "\C-m" 'newline-and-indent)
  (define-key c++-mode-map "\C-cc" 'compile)
  (define-key c++-mode-map "\C-cr" 'replace-string)
  
  ;; make parens show the text before the paren in the minibuffer
  (setq paren-backwards-message t)
  
  ;; setup some compile stuff
  (add-hook 'c-mode-hook
	    '(lambda () (or (file-exists-p "makefile") (file-exists-p "Makefile")
			    (progn 
			      ;; make parens show the text before the paren in the minibuffer
			      (setq paren-backwards-message t)
			      
			      (make-local-variable 'compile-command)  
			      (setq compile-command
				    (concat "make "
					    buffer-file-name))))))

  (c-set-offset 'class-close -2)
  ;;(c-set-offset 'c-brace-offset -2)
  (setq c-block-comment-prefix "* ")

  (add-special-font-lock-faces-jps (list 'c-font-lock-keywords 'c-font-lock-keywords-1 'c-font-lock-keywords-2 'c-font-lock-keywords-3))
  (add-special-font-lock-faces-jps (list 'c++-font-lock-keywords 'c++-font-lock-keywords-1 'c++-font-lock-keywords-2 'c++-font-lock-keywords-3))
  )

(add-hook 'c-mode-common-hook 'c-mode-common-hook-jps)

(defun c++-insert-header ()
  "Insert header denoting C++ code at top of buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert "// "
            "Hey, XEmacs this is "
            "-*- C++ -*-"
            "\n\n")))

;; Tell cc-mode not to check for old-style (K&R) function declarations.
;; This speeds up indenting a lot.
;;(setq c-recognize-knr-p nil)


;;;;;;;;;;;
;;
;; compile
;;
;;;;;;;;;;;;
(message "Compile")
(setq compilation-always-signal-completion nil
      compilation-read-command nil
      compile-highlight-display-limit 1024
      compilation-ask-about-save nil)
(if running-xemacs
    (add-hook 'compilation-mode-hook 'turn-off-font-lock))

;;(defadvice compile-internal (around compile-internal-jps)
;;  "Switch to compilation buffer in other window"
;;  (let (outbuf-jps (get-buffer-create
;;		    (funcall (or name-function compilation-buffer-name-function
;;				 (function (lambda (mode)
;;					     (concat "*" (downcase mode) "*"))))
;;			     name-of-mode))
;;		   (setq ad-return-value ad-do-it)
;;  (switch-to-buffer-other-window compilation-last-buffer)
;;  )
  
;;;;;;;;;;;
;;
;; EDiff
;;
;;;;;;;;;;;;
(message "EDiff")
;;HACK to get ediff to work
(defun ediff-file-remote-p (file-name)
  nil)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq-default ediff-ignore-similar-regions t)
(setq-default ediff-auto-refine t)

(if running-xemacs
    ;; TODO put in until xemacs gets fixed
    (setq ediff-coding-system-for-write 'escape-quoted))

;;;;;;;;;;;
;;
;; Perl
;;
;;;;;;;;;;;;
(message "Perl")
(require 'cperl-mode)
(if running-xemacs 
    (add-special-font-lock-faces-jps (list 'perl-font-lock-keywords 'perl-font-lock-keywords-1 'perl-font-lock-keywords-2)))

(defun cperl-mode-hook-jps ()
  ;; make parens show the text before the paren in the minibuffer
  (setq paren-backwards-message t)

  ;;(setq tab-width 2)
  (setq indent-tabs-mode nil)
  (local-set-key (concat prefix-key-jps "p") 'cperl-perldoc)
  )
(add-hook 'cperl-mode-hook 'cperl-mode-hook-jps)
(add-to-list 'auto-mode-alist '("\\.cgi$" . perl-mode))

;;;;;;;;;;;
;;
;; Java
;;
;;;;;;;;;;;;
(message "Java")

;; Load JDE
;;(add-to-list 'load-path (expand-file-name "~/.xemacs/jde-2.3.5/lisp"))
;;(load-file "~/.xemacs/jde-2.3.5/lisp/jde-autoload.el")

;; Load CEDET
;;(load-file "~/.xemacs/cedet/common/cedet.el")
;; Enabling SEMANTIC minor modes.  See semantic/INSTALL for more ideas.
;;(semantic-load-enable-minimum-features)

;; (add-to-list 'auto-mode-alist '("\\.jass$" . jde-mode))
;; (add-to-list 'auto-mode-alist '("\\.jad$" . jde-mode))
;; (add-to-list 'auto-mode-alist '("\\.xjava$" . jde-mode))
;; (add-to-list 'auto-mode-alist '("\\.groovy$" . jde-mode))
;; (add-to-list 'auto-mode-alist '("\\.astub$" . jde-mode))

(add-to-list 'auto-mode-alist '("\\.jass$" . java-mode))
(add-to-list 'auto-mode-alist '("\\.jad$" . java-mode))
(add-to-list 'auto-mode-alist '("\\.xjava$" . java-mode))
(add-to-list 'auto-mode-alist '("\\.groovy$" . java-mode))
(add-to-list 'auto-mode-alist '("\\.astub$" . java-mode))

;;basic setup
(setq java-home (getenv "JAVA_HOME"))

(defun java-mode-hook-jps ()
  ;; make parens show the text before the paren in the minibuffer
  (setq paren-backwards-message t)
  
  (define-key java-mode-map "\C-cc" 'compile)
  (define-key java-mode-map (concat prefix-key-jps "l") 'insert-class-name-jps)
  (define-key java-mode-map (concat prefix-key-jps "e") 'check-java-imports-jps)
  (define-key java-mode-map "\C-cr" 'replace-string)
  (c-set-offset 'inexpr-class 0)	;Don't indent inner classes too much
  (c-set-offset 'class-close 'c-lineup-close-paren) ;Line up end of class

  ;;setup some registers
  (set-register ?d "if(LOG.isDebugEnabled()) {")
  (set-register ?n "System.getProperty(\"line.separator\")")
  )
(add-hook 'java-mode-hook 'java-mode-hook-jps)

;ignore assert files from ant compilation
(add-to-list 'completion-ignored-extensions ".assert")

;;(when (eq system-type 'windows-nt)
;;  (eval-after-load "jde-run"
;;    ;;fix bug with wrong signal being sent to running processes
;;    (define-key jde-run-mode-map "\C-c\C-c" 'comint-kill-subjob)))


 ;;don't jump to the first error or remove the compilation buffer! 
(defadvice jde-compile-finish-kill-buffer (around remove-jde-compile-finish-kill-buffer)
  "remove jde-compile-finish-kill-buffer"
  )

(defun jde-mode-hook-jps()
  
  ;; make parens show the text before the paren in the minibuffer
  (setq paren-backwards-message t)
  
  ;;(modify-syntax-entry ?_ " ")
  (diminish 'senator-minor-mode "Sen")
  ;;(senator-minor-mode nil)
  
  ;;cperl-mode seems to screw this one up, so just make it buffer local and
  ;;set to nil
  (make-variable-buffer-local 'fill-paragraph-function)
  (setq fill-paragraph-function nil)

  (add-special-font-lock-faces-jps
   (list 'java-font-lock-keywords
	 'java-font-lock-keywords-1
	 'java-font-lock-keywords-2
	 'java-font-lock-keywords-3
	 'java-font-lock-keywords-4))

  (local-set-key [(control ?c) (control ?v) (control ?i)] 'jde-import-organize-jps)
  ;;(local-set-key [(control ?c) (control ?v) (control ?z)] 'jde-import-then-organize-jps)
  (local-set-key (concat prefix-key-jps "p") 'insert-project-header-info-jps)
  )
(add-hook 'jde-mode-hook 'jde-mode-hook-jps)

(if running-xemacs
    (add-hook 'jde-run-mode-hook 'turn-off-font-lock))

(defvar project-header-info nil "Information about a project for the header, usually the charge number and date")
(defun insert-project-header-info-jps ()
  (interactive)
  (insert project-header-info))

(defun jde-import-then-organize-jps ()
  (interactive)
  (call-interactively 'jde-import-find-and-import)
  (jde-import-organize-jps))

(defun jde-import-organize-jps ()
  (interactive)
  (save-excursion
    (jde-import-organize t)))

;; required by cedet and defined by JDE.  This makes sure it gets defined
;; up front.
(unless (fboundp 'subst-char-in-string)
  (defun subst-char-in-string (fromchar tochar string &optional inplace)
    "Replace FROMCHAR with TOCHAR in STRING each time it occurs.
Unless optional argument INPLACE is non-nil, return a new string."
    (let ((i (length string))
	  (newstr (if inplace string (copy-sequence string))))
      (while (> i 0)
	(setq i (1- i))
	(if (eq (aref newstr i) fromchar)
	    (aset newstr i tochar)))
      newstr)))


;;(defadvice jde-run-executable (around fix-for-process-connection-type-0)
;;  "Fix process type to be pipes for java"
;;  (let ((process-connection-type nil))
;;    (setq ad-return-value ad-do-it)))
;;
;;(defadvice jde-run-vm-launch (around fix-for-process-connection-type-1)
;;  "Fix process type to be pipes for java"
;;  (let ((process-connection-type nil))
;;    (setq ad-return-value ad-do-it)))
;;
;;(defadvice jde-ant-build (around fix-for-process-connection-type-2)
;;  "Fix process type to be pipes for java"
;;  (let ((process-connection-type nil))
;;    (setq ad-return-value ad-do-it)))

(defun insert-class-name-jps ()
  (interactive)
  (insert (replace-in-string (file-name-nondirectory (buffer-file-name))
			     ".java" "")))

(defun check-java-imports-jps ()
  (interactive)
  (compile (concat "imports.pl "
		   (get-java-file-jps))))

(defun get-java-file-jps ()
  (interactive)
  (file-name-nondirectory (expand-file-name buffer-file-name)))

;;;;;;;;;;;
;;
;; SGML
;;
;;;;;;;;;;;;
(message "SGML")
(defun sgml-mode-hook-jps ()
  (setq indent-tabs-mode nil)
  (font-lock-mode)
  (setq sgml-indent-data t) ;;for some reason this doesn't work right
  (auto-fill-mode -1)
  )
(add-hook 'sgml-mode-hook  'sgml-mode-hook-jps)

(make-face 'sgml-comment-face)
(if running-xemacs
    (set-face-parent 'sgml-comment-face 'default))
(set-face-foreground 'sgml-comment-face "darkblue")
(make-face 'sgml-start-tag-face)
(if running-xemacs
(set-face-parent 'sgml-start-tag-face 'default))
(set-face-foreground 'sgml-start-tag-face "black")
(make-face 'sgml-end-tag-face)
(if running-xemacs (set-face-parent 'sgml-end-tag-face 'default))
(set-face-foreground 'sgml-end-tag-face "SeaGreen")
(make-face 'sgml-entity-face)
(if running-xemacs (set-face-parent 'sgml-entity-face 'default))
(set-face-foreground 'sgml-entity-face "Red")
(make-face 'sgml-doctype-face)
(if running-xemacs (set-face-parent 'sgml-doctype-face 'default))
(set-face-foreground 'sgml-doctype-face "White")

;;my own catalog for dtds
(cond (running-xemacs
(require 'psgml)
;(add-to-list 'sgml-catalog-files
;	     (expand-file-name "CATALOG" (locate-data-directory "config-jps")))

(setq sgml-auto-activate-dtd nil	; don't parse dtd right away
      sgml-warn-about-undefined-elements nil ; don't complain about unknown elements
      sgml-warn-about-undefined-entities nil ; don't complain about unknown entities
      )

(setq sgml-set-face t)			; without this, all SGML text is in same color
(setq sgml-markup-faces
      '((comment   . sgml-comment-face)
	(start-tag . sgml-start-tag-face)
	(end-tag   . sgml-end-tag-face)
	(doctype   . sgml-doctype-face)
	(entity    . sgml-entity-face)))
))
(autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
(autoload 'xml-mode "psgml" "Major mode to edit XML files." t)

(add-to-list 'auto-mode-alist '("\\.xsd$" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.qrc$" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.wxs$" . xml-mode))


;;;;;;;;;;;
;;
;; conf-mode
;;
;;;;;;;;;;;;
(if running-xemacs
    (progn
      (autoload 'conf-mode "conf-mode" "Major mode to edit configuration files." t)
      (add-to-list 'auto-mode-alist '("\\.ini$" . conf-mode))))

;;;;;;;;;;;
;;
;; TAGS
;;
;;;;;;;;;;;;
(message "TAGS")
(eval-after-load
    "etags"
  '(progn
     (if (not (boundp 'tag-table-alist))
	 (setq tag-table-alist '()))
     (add-to-list 'tag-table-alist '("\\.el$" . "~/elib/"))
     (add-to-list 'tag-table-alist '("\\.emacs" . "~/elib/"))
     (add-to-list 'tag-table-alist '("" . "."))

     (setq tags-auto-read-changed-tag-files t)
     (setq tags-build-completion-table nil)
     t))

;;;;;;;;;;;
;;
;; display-time
;;
;;;;;;;;;;;;
(message "display-time")

;; display time and balloon stuff
;;(load-library "balloon-help")
;;(balloon-help-mode 1)
(setq display-time-balloon-show-mail-from nil
      display-time-24hr-format t
      display-time-day-and-date t
      display-time-echo-area nil
      display-time-mail-file t ; anything other than nil or a string will work
      )
;;(setq display-time-form-list '(time-text load-text mail-text))
(setq display-time-form-list '(load-text))
(display-time)
;;(display-time-stop)


;;;;;;;;;;;
;;
;; Crypt
;;
;;;;;;;;;;;;
(cond (running-xemacs
(message "Crypt")
(setq crypt-encryption-type 'pgp
      crypt-confirm-password t
      ;;crypt-never-ever-decrypt t ; handy if never encrypting stuff
      crypt-inhibit-formats '() ;; always decrypt stuff, this variable
				;; contains dos if mule exists
      )
(require 'crypt)
))

;;;;;;;;;;;
;;
;; minibuffer
;;
;;;;;;;;;;;;
(cond (running-xemacs
(message "minibuffer")

;; resize the minibuffer when stuff is too big
(resize-minibuffer-mode 1)
(setq resize-minibuffer-window-exactly nil)
))

;;;;;;;;;;;
;;
;; Manual mode
;;
;;;;;;;;;;;;
(message "Manual mode")

;; Set the colors for manual mode
(defun my-Manual-mode-hook ()
  ;; Set colors for things in Manual-mode
  (set-face-foreground 'man-bold "white")
  (set-face-foreground 'man-heading "blue")
  (set-face-foreground 'man-italic "steelblue")
  (set-face-foreground 'man-xref "darkgreen")
  (setq Man-notify-method 'aggressive) ; focus on the man page after formatting
  )
;;(set-face-foreground 'font-lock-comment-face "red")      
(add-hook 'Manual-mode-hook 'my-Manual-mode-hook)


;;;;;;;;;;;
;;
;; Antlr
;;
;;;;;;;;;;;;
(message "ANTLR")
(autoload 'antlr-mode "antlr-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.g$" . antlr-mode))
(add-hook 'speedbar-load-hook		; would be too late in antlr-mode.el
	  (lambda () (speedbar-add-supported-extension ".g")))

(defconst c-Java-access-key nil)	;antlr-mode references this, but it's not defined anywhere
(setq antlr-tab-offset-alist
      '((antlr-mode nil 2 nil)
	(java-mode "antlr" 2 nil)
	))
(defun antlr-mode-hook-jps()
  ;; make parens show the text before the paren in the minibuffer
  (setq paren-backwards-message t)
  
  (make-local-variable 'c-basic-offset)
  (setq c-basic-offset 2)
  (setq fill-column 78)
  (setq indent-tabs-mode nil)
  (turn-on-font-lock)
  )
(add-hook 'antlr-mode-hook 'antlr-mode-hook-jps)


;;;;;;;;;;;
;;
;; Python
;;
;;;;;;;;;;;;
(defun python-mode-hook-jps ()
  (setq py-indent-offset 4)
  )
(add-hook 'python-mode-hook 'python-mode-hook-jps)

;;;;;;;;;;;
;;
;; conf mode
;;
;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-unix-mode))


;;;;;;;;;;;
;;
;; my nifty functions
;;
;;;;;;;;;;;;
;;
;; handy methods
(require 'cl)
(defun kill-buffers-by-file-pattern (pattern)
  "Kill all buffers that have a filename containing the string PATTERN"
  (interactive "MFilename pattern: ")
    (let ((to-remove
	   (remove-if-not
	    '(lambda (buff)
	       (with-current-buffer buff
		 (let ((fname (if (eq major-mode 'dired-mode)
				  dired-directory
				(buffer-file-name buff))))
		   (and fname (string-match pattern fname)))))
	    (buffer-list))))
      (when to-remove
	(kill-some-buffers to-remove))))

(defun global-change-directory (from to)
  "Change directory of all buffers with default-directory FROM to TO."
  (interactive "DGlobally change directory from: \nDTo: ")
  (let ((bufs (buffer-list))
	(from (expand-file-name from)))
    (while bufs
      (with-current-buffer (car bufs)
	(when (equal from (expand-file-name default-directory))
	  (setq default-directory to)))
      (setq bufs (cdr bufs)))))

(defun raw-unix ()
  "Set buffer-file-coding-system to raw-text (unix)."
  (interactive)
  (setq buffer-file-coding-system 'raw-text)
  )

(defun switch-to-shell-jps ()
  "Switch to the shell buffer"
  (interactive)
  (if (null (get-buffer "*shell*"))
      (shell)
    (switch-to-buffer "*shell*"))
  )

;;(defun create-new-shell-jps ()
;;  "Create a new shell buffer if *shell* already exists, otherwise create the base shell buffer"
;;  (interactive)
;;  (if (null (get-buffer "*shell*"))
;;      (shell)
;;    (progn
;;      (switch-to-buffer "*shell*")
;;      (let ((temp-name (rename-buffer "*shell*-temp" t)))
;;	(shell)
;;	(rename-uniquely)
;;	(switch-to-buffer temp-name)
;;	(rename-buffer "*shell*")
;;	)
;;  )

(defun revert-buffer-jps () 
  "Revert the current buffer with no questions asked"
  (interactive)
  (revert-buffer t t nil))


;; ASCII table
(defun ascii-table ()
  "Display a list of the first 128 ASCII chars and keyboard equivalents."
  (interactive)
  (let ((char 0)
	(next-line-add-newlines-save next-line-add-newlines))
    (message "Making the ascii table...")
    (setq next-line-add-newlines t)
    (save-excursion
      (set-buffer (get-buffer-create "*ASCII Table*"))
      (setq buffer-read-only nil)
      (erase-buffer)
      (goto-char (point-min))
      (while (<= char 127)
	(insert (format "%d %s\t" char (single-key-description char)))
	(setq char (1+ char))
	(if (>= (count-lines (point-min) (point)) 13)
	    (goto-char (point-min))
	  (next-line 1))
	(end-of-line))
      (goto-char (point-min))
      ;; this may be overkill, but it is the quickest way I know to nuke
      ;; blank space at the end of all the lines in a buffer.
      ;;(picture-mode) (picture-mode-exit)
      (setq buffer-read-only t))
    (setq next-line-add-newlines next-line-add-newlines-save)
    (display-buffer "*ASCII Table*")
    (message "Making the ascii table...done")))

(defun ascii-table-octal ()
  "Display a list of the first 128 ASCII chars and keyboard equivalents."
  (interactive)
  (let ((char 0)
	(next-line-add-newlines-save next-line-add-newlines))
    (message "Making the ascii table...")
    (setq next-line-add-newlines t)
    (save-excursion
      (set-buffer (get-buffer-create "*ASCII Table Octal*"))
      (setq buffer-read-only nil)
      (erase-buffer)
      (goto-char (point-min))
      (while (<= char 127)
	(insert (format "%o %s\t" char (single-key-description char)))
	(setq char (1+ char))
	(if (>= (count-lines (point-min) (point)) 13)
	    (goto-char (point-min))
	  (next-line 1))
	(end-of-line))
      (goto-char (point-min))
      ;; this may be overkill, but it is the quickest way I know to nuke
      ;; blank space at the end of all the lines in a buffer.
      (picture-mode) (picture-mode-exit)
      (setq buffer-read-only t))
    (setq next-line-add-newlines next-line-add-newlines-save)
    (display-buffer "*ASCII Table Octal*")
    (message "Making the ascii table...done")))


;;;;;;;;;;;
;;
;; Buffer list
;;
;;;;;;;;;;;;
(message "Buffer list")
;; fancy buffer list
(require 'bs)
(setq bs-default-configuration "all")
;;(setq bs-buffer-sort-function 'bs-sort-buffer-interns-are-last)
(setq bs-buffer-sort-function 'bs--sort-by-mode)
(setq bs-dont-show-function nil)


;;;;;;;;;;;
;;
;; Save Place
;;
;;;;;;;;;;;;
(message "Save Place")
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.xemacs/saved-places")
; may speed up emacs exit (setq save-place-forget-unreadable-files nil)

;;;;;;;;;;;
;;
;; Backups
;;
;;;;;;;;;;;;
(when (or running-xemacs running-gnuemacs)
  (message "Backups")

  ;; 4/29/2023 - I can't remember the last time I used an emacs backup file
  (setq make-backup-files nil)
  
 ;;  (setq make-backup-files t
 ;;        backup-by-copying t		; don't clobber symlinks
 ;;        delete-old-versions t
 ;;        backup-directory-alist '((".*" . "~/.xemacs/backups"))
 ;;        )
 ;;  
  ;; (setq auto-save-directory (expand-file-name "~/.xemacs/auto-save/")
  ;;       auto-save-directory-fallback auto-save-directory
  ;;       auto-save-hash-p nil
  ;;       ;; now that we have auto-save-timeout, let's crank this up
  ;;       ;; for better interactive response.
  ;;       auto-save-interval 2000
  ;;       )
  ;; ;; We load this afterwards because it checks to make sure the
  ;; ;; auto-save-directory exists (creating it if not) when it's loaded.
  ;; (when running-xemacs
  ;;   (require 'auto-save)
  ;;   )
  )

(when (and
       running-unix
       (fboundp 'gnuserv-start)
       )
  ;;(setq gnuserv-frame t);;Use the current frame for gnuserv clients, Setting this causes gnuclient to not work correctly!
  (gnuserv-start)
  )

(when (and
       running-unix
       (fboundp 'server-start)
       )
  ;;(setq gnuserv-frame t);;Use the current frame for gnuserv clients, Setting this causes gnuclient to not work correctly!
  (server-start)
  )

;; dictionary and thesaurus
(autoload 'dict "dict" nil t)
(autoload 'thesaurus "dict" nil t)


;;;;;;;;;;;
;;
;; icomplete - replaces iswitchb
;;
;;;;;;;;;;;;
;(icomplete-mode 1)
;(eval-after-load "icomplete" '(progn (require 'icomplete+)))
;(icompletep-cycling-mode 1)


;;;;;;;;;;;
;;
;; Tramp
;;
;;;;;;;;;;;;
(message "Tramp")

;; installed in standard location, should not need these calls 
;;(require 'tramp)
;;(load-library "tramp") ; otherwise variables below don't exist

;;(with-eval-after-load "tramp"
(eval-after-load "tramp"
  (lambda ()
    (message "tramp has been loaded, setting variables")
    ;;(setq tramp-default-method "ssh") ; default is scp
 
    ;; TRAMP gets confused by my prompt some times, so make sure it's
    ;; simple for THIS Emacs process, and therefore subprocesses.
    ;;(setenv "PS1" "tramp@\h> ")
 
    ;; disable backups of files edited with tramp
    ;;(setq tramp-bkup-backup-directory-info  nil)

    ;; pickup the path from the remote system
    ;;(add-to-list 'tramp-remote-path 'tramp-own-remote-path)    
    ))



;;; --------------------
;;; -- ido - buffer completion and other stuff
;;; http://www.emacswiki.org/emacs/InteractivelyDoThings
;;(unless (fboundp 'called-interactively-p)
;;  (defun called-interactively-p (kind)
;;    nil))
;;(ido-mode)
;;(setq ido-default-buffer-method 'selected-window)

;;;;;;;;;;;
;;
;; Uniquify
;;
;; load this at the end to make sure everything it caches is up to date,
;; such as directory-sep-char
;;
;;;;;;;;;;;;
(message "uniquify")

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
;; don't change the name until after buffer is deleted
;;(add-hook 'post-command-hook 'delay-uniquify-rationalize-file-buffer-names)


;;;;;;;;;;;
;;
;; Spell
;;
;;;;;;;;;;;;
(cond ((eq system-type 'darwin)
       (setq ispell-program-name "/usr/local/brew/bin/aspell"))
      (t
       (setq ispell-program-name "aspell"))
)

;;;;;;;;;;;;
;;; -- PCL-CVS
;;;;;;;;;;;;

;;;; Fix parsing of commit messages that were broken with OpenCVS version 1.12.9 
(defadvice cvs-parse-commit (around ede-pcl-cvs-parse-commit) 
  "Fix parsing of commit messages that were broken with OpenCVS version 1.12.9" 
  ;; don't call original - replace with my version 
  (setq ad-return-value (ede-cvs-parse-commit))
  ) 

(defun cvs-get-local-commit-root () 
  "Return the current repository-local commit root.  
That is, the cvsroot as seen on the cvs server (if remote), without hostname if any but with the module name appended" 
  (let ((root (cvs-get-cvsroot)) 
        (module (cvs-get-module))) 
    (if (and root module) 
      (if (string-match "\\`.*:\\([^:]+\\)\\'" root) 
          (concat (match-string 1 root) "/" module) 
        (concat root "/" module)))))
  
(defun ede-cvs-parse-commit () 
  (let ((root (cvs-get-local-commit-root)) 
        path base-rev subtype) 
  
    ;;(log-message "CVS" (concat "root: " root)) 
    ;;(log-message "CVS" (concat "matching: '"  
    ;;                           (regexp-quote root) "/" "\\(.*\\),v  <--  .*$" 
    ;;                           "'")) 
    (cvs-or 
     (and 
      ;; eat obsolete "Checking in" comment - lost in OpenCVS version 1.12.9 
      (cvs-or (cvs-match "\\(Checking in\\|Removing\\) \\(.*\\);$") t) 
      ;; Instead, match on repository-local commit-root prefix 
      (cvs-match (concat (regexp-quote root) "/" "\\(.*\\),v  <--  .*$") 
                 (path 1)) 
      ;;(cvs-or (log-message "CVS" (concat "found: " path)) t) 
      (cvs-or 
       ;; deletion 
       (cvs-match "new revision: delete; previous revision: \\([0-9.]*\\)$" 
    (subtype 'REMOVED) (base-rev 1)) 
       ;; addition 
       (cvs-match "initial revision: \\([0-9.]*\\)$" 
    (subtype 'ADDED) (base-rev 1)) 
       ;; update 
       (cvs-match "new revision: \\([0-9.]*\\); previous revision: .*$" 
    (subtype 'COMMITTED) (base-rev 1))) 
      ;; eat obsolete "done" comment - lost in OpenCVS version 1.12.9 
      (cvs-or (cvs-match "done$") t) 
      ;; it's important here not to rely on the default directory management 
      ;; because `cvs commit' might begin by a series of Examining messages 
      ;; so the processing of the actual checkin messages might begin with 
      ;; a `current-dir' set to something different from "" 
      (cvs-parsed-fileinfo (cons 'UP-TO-DATE subtype) path 'trust 
      :base-rev base-rev)) 
      
     ;; useless message added before the actual addition: ignored 
     (cvs-match "RCS file: .*\ndone$"))))

;;;;;;;;;;;;
;;; csharp mode
;;;;;;;;;;;;
(autoload 'csharp-mode "csharp-mode-0.4.0" "csharp mode" t)

(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))

(defun buffer-untabify ()
  "Untabify an entire buffer"
  (interactive)
  (untabify (point-min) (point-max)))

;;Included in XEmacs 21.5.6, but not my current version
(unless (fboundp 'count-screen-lines)
  (defun count-screen-lines (&optional beg end count-final-newline window)
    "Return the number of screen lines in the region.
The number of screen lines may be different from the number of actual lines,
due to line breaking, display table, etc.

Optional arguments BEG and END default to `point-min' and `point-max'
respectively.

If region ends with a newline, ignore it unless optional third argument
COUNT-FINAL-NEWLINE is non-nil.

The optional fourth argument WINDOW specifies the window used for obtaining
parameters such as width, horizontal scrolling, and so on.  The default is
to use the selected window's parameters.

Like `vertical-motion', `count-screen-lines' always uses the current buffer,
regardless of which buffer is displayed in WINDOW.  This makes possible to use
`count-screen-lines' in any buffer, whether or not it is currently displayed
in some window."
    (unless beg
      (setq beg (point-min)))
    (unless end
      (setq end (point-max)))
    (if (= beg end)
	0
      (save-excursion
	(save-restriction
	  (widen)
	  (narrow-to-region (min beg end)
			    (if (and (not count-final-newline)
				     (= ?\n (char-before (max beg end))))
				(1- (max beg end))
			      (max beg end)))
	  (goto-char (point-min))
	  (1+ (vertical-motion (buffer-size) window))))))
  )

;;;;;;;;;;;
;;
;; cmake
;;
;;;;;;;;;;;;
; Add cmake listfile names to the mode list.
(setq auto-mode-alist
	  (append
	   '(("CMakeLists\\.txt\\'" . cmake-mode))
	   '(("\\.cmake\\'" . cmake-mode))
	   auto-mode-alist))

(autoload 'cmake-mode "cmake-mode" nil t)

;;;;;;;;;;;
;;
;; Markdown
;;
;;;;;;;;;;;;
;;(require 'markdown-mode)

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
;(autoload 'gfm-mode "gfm-mode"
;  "Major mode for editing GitHub Markdown files" t)
;(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
;(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
(add-hook 'markdown-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)
	    (if (fboundp 'check-parens)
		
		(when buffer-file-name
		  (add-hook 'after-save-hook
			    'check-parens
			    nil t)))))
;;(setq markdown-command "pandoc --from gfm --to html --standalone")

;;;;;;;;;;;
;;
;; Diminish
;;
;; put at end so everything is loaded
;;
;;;;;;;;;;;;
(cond (running-xemacs
(message "diminish")
(require 'diminish)
(require 'compile)
(require 'lazy-lock)
(diminish 'compilation-in-progress "C")
(diminish 'compilation-minor-mode "C")
(diminish 'abbrev-mode "Abv")
(diminish 'font-lock-mode "F")
(diminish 'lazy-lock-mode "L")
(diminish 'auto-fill-function "Af")
(diminish 'isearch-mode "IS")
(diminish 'filladapt-mode "Fa")
))

;;; Emacs compatibility
(unless (or (fboundp 'quit-window) (boundp 'quit-window))
  (defalias 'quit-window 'kill-this-buffer))

;;HACK Something is screwed up, but this fixes it
(when (not (boundp 'null-buffer-file-name)) (defun null-buffer-file-name ()))

;; make sure to always split windows vertically
(setq split-width-threshold 80000)

;; dired switch on some Linux distros doesn't work
(if (and running-xemacs
	 (or (eq system-type 'linux) (eq system-type 'gnu/linux)))
    (setq dired-use-ls-dired nil))

;; go-mode
(add-hook 'go-mode-hook
	  '(lambda ()
	     ;; make parens show the text before the paren in the minibuffer
	     (setq paren-backwards-message t)
	     (setq indent-tabs-mode nil);; no tabs in source code
	     (setq tab-width 2)
	     ))

;; yaml-mode
;;(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(add-hook 'yaml-mode-hook
          (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; applescript
(autoload 'applescript-mode "applescript-mode"
  "Major mode for editing AppleScript source." t)
(add-to-list 'auto-mode-alist '("\\.applescript$" . applescript-mode))

;; something for Aquamacs
(if (not (boundp 'display-info))
    (setq display-info nil))


;; pandoc mode

(setq pandoc-data-dir "~/.xemacs/xemacs-packages/etc/pandoc-mode")
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

;; cua mode
(setq cua-enable-cua-keys nil)  ; enable only CUA's rectangle selections
(cua-mode t)


;; elpy mode
(setq python-shell-interpreter "python3"
      python-shell-interpreter-args "-i")
(setq elpy-rpc-python-command python-shell-interpreter)

;; prompt before exit
(defun ask-before-closing ()
  "Close only if y was pressed."
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to close emacs? "))
      (save-buffers-kill-emacs)                                                                                            
    (message "Canceled frame close")))

(global-set-key (kbd "C-x C-c") 'ask-before-closing)

;; HELP-mode
(setq help-window-select t) ; always focus on the help buffer after asking for help

;; manage the trash
(require 'trashed)
(global-set-key (concat prefix-key-jps "t") 'trashed)

;;;stuff emacs likes to append on it's own
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(setq minibuffer-max-depth nil)

;; END
(message "done loading configuration")

