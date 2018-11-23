LISPDIR := site-lisp

EMACS   ?= emacs-$(shell basename `pwd`)

.PHONY: all build clean
all: #build
	echo $(EMACS)
	echo $(REPOS)

.make-elc-%:
	$(EMACS) -batch -f batch-byte-compile $(*:.elc=.el)

.make-repo-%:
	$(if $(realpath $(LISPDIR)/$*/Makefile), \
	  EMACS=$(EMACS) $(MAKE) -C $(LISPDIR)/$*)

