all:
	mkdir -p auto-save
	mkdir -p cache
	mkdir -p backups
	make -C cedet-1.0beta3b
	make -C jde-2.3.5/lisp
	make -C xemacs-packages/lisp
