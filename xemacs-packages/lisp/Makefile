EMACS = xemacs

ELCS=$(patsubst %.el,%.elc,$(wildcard *.el))

%.elc: %.el
	$(EMACS) -batch -f batch-byte-compile $<

.PHONY: all
all: camelCase $(ELCS)

.PHONY: camelCase
camelCase:
	make -C camelCase

x-migrant.elc: x-migrant.el
	$(EMACS) -batch -l $< -f batch-byte-compile $<

clean:
	rm -f *.elc
