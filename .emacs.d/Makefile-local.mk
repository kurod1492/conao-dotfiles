MIXFILE ?= mixed.el
EMACS   ?= emacs-$(shell basename `pwd`)

.PHONY: all build clean
all: build
	echo $(EMACS)
	echo $(REPOS)

build:
	$(EMACS) -batch -f batch-byte-compile $(MIXFILE)

clean:
	-rm rf $(MIXFILE)
