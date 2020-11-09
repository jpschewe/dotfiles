(require 'package)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(mapc #'(lambda (package)
	  (unless (package-installed-p package)
	    (package-install package)))
      '(
	ascii-table
	php-mode
 	applescript-mode
 	cargo
 	csharp-mode
 	diminish
 	elpy
 	go-mode
 	markdown-mode
 	osx-clipboard
 	rustic
 	yaml-mode
 	))
