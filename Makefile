all:

include Makefunc.mk

TOP_DIR  := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
HOME_DIR ?= ~

.PHONY: all install
all:
	@echo $(TOP_DIR)
	@echo "Install dotfiles in \$$HOME_DIR."
	@echo "Install in $$HOME by default, but if you want install"
	@echo "specific folder, run as below.  (do not write slash at end)"
	@echo
	@echo "\$$ make install HOME_DIR=~/conao-dotfiles/debug"
	@echo

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
