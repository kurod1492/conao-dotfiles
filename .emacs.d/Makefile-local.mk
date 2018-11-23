EMACS ?= emacs-$(shell basename `pwd`)

all:
	echo $(EMACS)
	echo $(REPOS)
