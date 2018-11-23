LISPDIR := site-lisp

EMACS   ?= emacs-$(shell basename `pwd`)

.PHONY: all build clean
all: #build
	echo $(EMACS)
	echo $(REPOS)

.make-elc-%:
	$(EMACS) -batch -f batch-byte-compile $(patsubst .make-elc-%.elc,%.el,$@)

.make-repo-%:
	$(if $(wildcard $(LISPDIR)/$(subst .make-repo-,,$@)/Makefile), \
	  EMACS=$(EMACS) $(MAKE) -C $(LISPDIR)/$(subst .make-repo-,,$@))

clean:
	-rm rf $(MIXFILE)
