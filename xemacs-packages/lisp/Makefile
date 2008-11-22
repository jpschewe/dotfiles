EMACS = xemacs

ELCS=$(patsubst %.el,%.elc,$(wildcard *.el))

%.elc: %.el
	$(EMACS) -batch -f batch-byte-compile $<

x-migrant.elc: x-migrant.el
	$(EMACS) -batch -l $< -f batch-byte-compile $<

.PHONY: all
all: camelCase $(ELCS)

.PHONY: camelCase
camelCase:
	make -C camelCase

clean:
	rm -f *.elc
