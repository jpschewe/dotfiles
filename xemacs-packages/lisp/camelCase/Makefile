EMACS = xemacs

all:
	rm -f *.elc
	$(EMACS) -batch -f batch-byte-compile `echo *.el`
