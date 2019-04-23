all:

include Makefunc.mk

HOME_DIR   ?= ~
SOURCE_DIR := .dotfiles
MAKE_DIRS  := .emacs.d
DOTFILES   := .emacs.d .bash_profile .bashrc

.PHONY: all install
all:
	$(MAKE) install -n
	@$(call ECHO_YELLOW,"If you really run above comamnds","\n\n","")
	@$(call ECHO_YELLOW,"run 'make install'.","","\n")

install: $(DOTFILES:%=$(HOME_DIR)/%) $(MAKE_DIRS:%=.make-make-%)
	@echo
	@echo "==== job completed ===="
	@echo

$(DOTFILES:%=$(HOME_DIR)/%):
	ln -sf $(SOURCE_DIR)/$(@F) ~/

.make-make-%:
	$(MAKE) -C $*

clean: $(MAKE_DIRS:%=.make-clean-%)
.make-clean-%:
	$(MAKE) -C $* clean
