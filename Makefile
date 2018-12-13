HOMEDIR   := ~
SOURCEDIR := .dotfiles
MAKEDIRS  := .emacs.d
DOTFILES  := .emacs.d .gitignore .gitconfig

.PHONY: all
all: $(DOTFILES:%=$(HOMEDIR)/%) $(MAKEDIRS:%=.make-make-%)
	@echo
	@echo "==== job completed ===="
	@echo

$(DOTFILES:%=$(HOMEDIR)/%):
	ln -sf $(SOURCEDIR)/$(@F) ~/

.make-make-%:
	$(MAKE) -C $(HOMEDIR)/$*
