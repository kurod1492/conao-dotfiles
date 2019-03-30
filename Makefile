all:

include Makefunc.mk

HOMEDIR   := ~
SOURCEDIR := .dotfiles
MAKEDIRS  := .emacs.d
DOTFILES  := .emacs.d .gitignore_global .gitconfig .bash_profile .bashrc 

.PHONY: all install
all:
	$(MAKE) install -n
	@$(call ECHO_YELLOW,"If you really run above comamnds","\n\n","")
	@$(call ECHO_YELLOW,"run 'make install'.","","\n")

install: $(DOTFILES:%=$(HOMEDIR)/%) $(MAKEDIRS:%=.make-make-%)
	@echo
	@echo "==== job completed ===="
	@echo

$(DOTFILES:%=$(HOMEDIR)/%):
	ln -sf $(SOURCEDIR)/$(@F) ~/

.make-make-%:
	$(MAKE) -C $*

clean: $(MAKEDIRS:%=.make-clean-%)
.make-clean-%:
	$(MAKE) -C $* clean
