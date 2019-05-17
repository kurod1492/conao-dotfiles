all:

include Makefunc.mk

GREP        ?= grep
LOCALDIR    := local
LISPDIR     := site-lisp
CONFDIR     := conf
BUILDDIR    := build
SNIPPETDIR  := snippets
TEMPLETEDIR := templete
TEXCACHEDIR := latex-math-preview-cache

MIXRAWNAME  := conao-mixed-raw.el
MIXFILENAME := conao-mixed.el
MIXELCNAME  := $(MIXFILENAME:%.el=%.elc)
MIXRAWFILE  := $(LOCALDIR)/$(MIXRAWNAME)
MIXFILE     := $(LOCALDIR)/$(MIXFILENAME)

GITHUB      := https://github.com
REMOTE      := $(GITHUB)/conao3
REPOS       := leaf.el orglyth.el cort-test.el seml-mode.el po-mode.el navbar.el
REPOS         += feather.el feather-server.el leaf-browser.el
REPODIRS    := $(addprefix $(LISPDIR)/, $(REPOS))

VERNAME_CMD := --version | head -n 1 | cut -d ' ' -f 3 | $(GREP) -oP '^\d+\.\d+'   
EMACS_RAW   := $(filter-out emacs-undumped, $(shell compgen -c emacs- | xargs))
ALL_EMACS   := $(strip $(sort $(EMACS_RAW)))
EMACS_VERS  := $(foreach ver,$(ALL_EMACS),$(shell $(ver) $(VERNAME_CMD)))
EMACS_DIC   := $(call MAKEDIC,$(ALL_EMACS),$(EMACS_VERS))

CONFFILES   := $(sort $(wildcard $(CONFDIR)/*.el))

LOCALDIRS   := $(addprefix $(LOCALDIR)/,$(EMACS_VERS))

SED_MIXFILE := 's/;.*//g' -e 's/(provide .*)//g' -e '/^ *$$/d'

LOCAL_BUILDDIRS   := $(addsuffix /$(BUILDDIR),$(LOCALDIRS))
LOCAL_MIXFILES    := $(addsuffix /$(MIXFILENAME),$(LOCALDIRS))
LOCAL_LISPDIRS    := $(addsuffix /$(LISPDIR),$(LOCALDIRS))
LOCAL_SNIPPETS    := $(addsuffix /$(SNIPPETDIR),$(LOCALDIRS))
LOCAL_TEMPLETES   := $(addsuffix /$(TEMPLETEDIR),$(LOCALDIRS))
LOCAL_TEXCACHES   := $(addsuffix /$(TEXCACHEDIR),$(LOCALDIRS))
LOCAL_REPOS       := $(foreach repo,$(REPOS),$(addsuffix /$(repo),$(LOCAL_LISPDIRS)))

LOCAL_MIXELCFILES := $(addsuffix /$(MIXELCNAME),$(LOCAL_BUILDDIRS))

DIRS          := $(LOCALDIR) $(LISPDIR) $(LOCALDIRS) $(LOCAL_BUILDDIRS) $(LOCAL_LISPDIRS)

CURRENT_BRANCH = $(shell cd $1; test -d .git && git branch | $(GREP) \* | cut -d ' ' -f2)
PULL_JOBS     := $(addprefix .make-pull-,$(REPOS))

EMACS_KEY2VAL  = $(call KEY2VAL,$(EMACS_DIC),$1)
EMACS_VAL2KEY  = $(call VAL2KEY,$(EMACS_DIC),$1)
LOCAL_EMACS    = $(call EMACS_VAL2KEY,$(call STRCUTREV,$@,'/',$1))

DEV_PKGS := leaf.el orglyth.el cort-test.el seml-mode.el po-mode.el navbar.el
DEV_PKGS  += feather.el feather-server.el leaf-browser.el

CLONE_URLS :=

EMACS_LST := 22.0 22.1 22.2 22.3
EMACS_LST  += 23.0 23.1 23.2 23.3 23.4
EMACS_LST  += 24.0 24.1 24.2 24.3 24.4 24.5
EMACS_LST  += 25.0 25.1 25.2 25.3
EMACS_LST  += 26.0 26.1 26.2
EMACS_LST  += 27.0
DIRS := site-lisp local $(EMACS_LST:%=local/%/site-lisp)

##################################################

.PHONY: all test build clean
all: $(DIRS) build
	@$(call ECHO_YELLOW,"make job:all completed!!","\n","\n")

$(DIRS):
	mkdir -p $@

build: clone

##############################

clone: $(DEV_PKGS:%=site-lisp/%)

site-lisp/%:
	if [ -d ~/dev/repos/$* ]; then \
	  ln -s ~/dev/repos/$* $@ \
	else \
	  git clone https://github.com/conao3/dotfiles.git $@ \
	fi

##########
_TARGETDIR = $(shell echo $@ | cut -d '/' -f 1,2)
_BYTECOMP = --batch -f batch-byte-compile $(<F)
$(LOCAL_MIXELCFILES): $(LOCAL_MIXFILES) $(REPODIRS)
	@$(call ECHO_MAGENTA,"compile $@...","\n","")
	-ln -sf $(shell readlink $<) $(@D)/
	-cd $(_TARGETDIR); $(call LOCAL_EMACS,2) $(REPODIRS:%=-L %) $(_BYTECOMP)
	-cp $(<:%el=%elc) $@

$(LOCAL_MIXFILES): $(MIXFILE)
	ln -fs ../$(<F) $@

##########
define build_repo
$(addsuffix /$(LISPDIR)/$1, $(LOCALDIRS)): $(LISPDIR)/$1
	@$$(call ECHO_MAGENTA,"compile $$@...","\n","")
	-rm -rf $$@
	cp -rf $$(LISPDIR)/$$(@F) $$@
	-EMACS=$$(call LOCAL_EMACS,2) $$(MAKE) -C $$@
endef
$(foreach repo,$(REPOS),$(eval $(call build_repo,$(repo))))

##########
$(LOCAL_SNIPPETS): $(SNIPPETDIR)
	ln -s ../../$(@F) $(LOCALDIR)/$(call STRCUTREV,$@,'/',1)/

$(LOCAL_TEMPLETES): $(TEMPLETEDIR)
	ln -s ../../$(@F) $(LOCALDIR)/$(call STRCUTREV,$@,'/',1)/

$(LOCAL_TEXCACHES): $(TEXCACHEDIR)
	ln -s ../../$(@F) $(LOCALDIR)/$(call STRCUTREV,$@,'/',1)/

#################### support jobs
##########
$(MIXFILE): $(CONFFILES)
	cat $^ > $(MIXRAWFILE)
	cat $(MIXRAWFILE) | sed -e $(SED_MIXFILE) > $(MIXFILE)
	echo "(provide 'conao-mixed)" >> $(MIXFILE)

$(REPODIRS):
	@$(call ECHO_MAGENTA,"git clone $(REMOTE)/$(@F)","\n","")
	git clone --depth 1 $(REMOTE)/$(@F) $@

# .make-update-. is update dotfiles(root) repository.
update: $(REPOS:%=.make-update-%) .make-update-.
	$(MAKE)

unshallow: $(REPOS:%=.make-unshallow-%)
	$(MAKE)

.make-update-%:
	@$(call ECHO_BLUE,"git pull origin (on $*)","\n","")
	cd $(LISPDIR)/$*; git pull origin $(call CURRENT_BRANCH,$(LISPDIR)/$*)

.make-unshallow-%:
	@$(call ECHO_BLUE,"git fetch --unshallow (on $*)","\n","")
	-cd $(LISPDIR)/$*; git fetch --unshallow

clean:
	-rm -rf $(DIRS)
	@$(call ECHO_CYAN,"make job:clean completed!!","\n","\n")
