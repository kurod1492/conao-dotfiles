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
REPOS         += feather.el feather-server.el leaf-browser.el straight.el
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


##################################################

.PHONY: all test build clean
all: $(DIRS) build
	@$(call ECHO_YELLOW,"make job:all completed!!","\n","\n")

test:
	echo $(call MAKEDIC,$(ALL_EMACS),$(EMACS_VERS))
	@echo $(EMACS_DIC)
	@echo $(call VAL2KEY,$(EMACS_DIC),22.1)

$(DIRS):
	mkdir -p $@

build: $(LOCAL_MIXELCFILES) $(LOCAL_REPOS) $(LOCAL_SNIPPETS) $(LOCAL_TEMPLETES) $(LOCAL_TEXCACHES)

#################### main jobs
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
