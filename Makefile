all:
	mkdir -p auto-save
	chmod go-rx auto-save
	mkdir -p cache
	chmod go-rx cache
	mkdir -p backups
	chmod go-rx backups
	make EMACS=/usr/X11R6/bin/xemacs -C cedet
	make -C jde-2.3.5.1/lisp
	make -C xemacs-packages/lisp

clean:
	#make -C cedet-1.0beta3b clean
	make EMACS=/usr/X11R6/bin/xemacs -C cedet-1.0pre3 clean
	make -C xemacs-packages/lisp clean
