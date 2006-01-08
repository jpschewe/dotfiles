EMACS = xemacs

all: camelCase
	rm -f *.elc
	$(EMACS) -batch -l x-migrant.el -f batch-byte-compile `echo *.el`

.PHONY: camelCase
camelCase:
	make -C camelCase

clean:
	rm -f *.elc
