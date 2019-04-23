all:

include Makefunc.mk

TOPDIR  := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
HOMEDIR ?= ~

DOTFILES     := $(filter-out ./,$(shell cd .dotfiles && git ls-tree --name-status HEAD)) 
CONFIG_DIRS  := $(filter-out ./,$(shell cd .config && git ls-tree -rd --name-status HEAD))
CONFIG_FILES := $(shell cd .config && git ls-tree -r --name-status HEAD)

DIRS := $(HOMEDIR) $(CONFIG_DIRS:%=$(HOMEDIR)/.config/%)

.PHONY: all install debug
all:
	@echo $(TOPDIR)
	@echo "Install dotfiles in \$$HOME_DIR."
	@echo "Install in $$HOME by default, but if you want install"
	@echo "specific folder, run as below.  (do not write slash at end)"
	@echo
	@echo "\$$ make install HOMEDIR=~/conao-dotfiles/debug"
	@echo

debug:
	$(MAKE) install HOMEDIR=$(TOPDIR)/.debug

##############################

install: $(DIRS) dotfiles config
	$(MAKE) -C $(HOMEDIR)/.emacs.d
	@echo
	@echo "==== job completed ===="
	@echo

$(DIRS):
	mkdir -p $@

##############################

dotfiles: $(DOTFILES:%=$(HOMEDIR)/%)
$(HOMEDIR)/%: $(TOPDIR)/.dotfiles/%
	ln -sf $< $@

config: $(CONFIG_FILES:%=$(HOMEDIR)/.config/%)
$(HOMEDIR)/.config/%: $(TOPDIR)/.config/%
	ln -sf $< $@

##############################

clean:
	rm -rf $(DOTFILES:%=$(HOMEDIR)/%) $(CONFIG_FILES:%=$(HOMEDIR)/.config/%)
