EMACS = xemacs

ELCS=$(patsubst %.el,%.elc,$(wildcard *.el))

%.elc: %.el
	$(EMACS) -batch -f batch-byte-compile $<

.PHONY: all
all: $(ELCS)

clean:
	rm -f *.elc
