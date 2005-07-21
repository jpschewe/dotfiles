all:
	mkdir -p auto-save
	chmod go-rx auto-save
	mkdir -p cache
	chmod go-rx cache
	mkdir -p backups
	chmod go-rx backups
	make -C cedet
	make -C jde-2.3.5/lisp
	make -C xemacs-packages/lisp
