LOCALDIR    := local
LISPDIR     := site-lisp
CONFDIR     := conf

MIXRAWNAME  := mixed-raw.el
MIXFILENAME := mixed.el
MIXELCNAME  := mixed.elc
MIXRAWFILE  := $(LOCALDIR)/$(MIXRAWNAME)
MIXFILE     := $(LOCALDIR)/$(MIXFILENAME)

GITHUB      := https://github.com
REMOTE      := $(GITHUB)/conao3
REPOS       := leaf.el orglyth.el cort.el
REPODIRS    := $(addprefix $(LISPDIR)/, $(REPOS))

EMACS_RAW   := $(filter-out emacs-undumped, $(shell compgen -c emacs- | xargs))
ALL_EMACS   := $(strip $(sort $(EMACS_RAW)))
EMACS_VERS  := $(subst emacs-,,$(ALL_EMACS))

CONFFILES   := $(CONFDIR)/*.el
MAKEFILE    := Makefile-local.mk

LOCALDIRS   := $(addprefix $(LOCALDIR)/, $(EMACS_VERS))
DIRS        := $(LOCALDIR) $(LISPDIR) $(LOCALDIRS)

SED_MIXFILE := 's/;.*//g' -e 's/(provide .*)//g' -e '/^$$/d'

LOCAL_MAKEFILES   := $(addsuffix /Makefile, $(LOCALDIRS))
LOCAL_MIXFILES    := $(addsuffix /$(MIXFILENAME), $(LOCALDIRS))
LOCAL_MIXELCFILES := $(addsuffix /$(MIXELCNAME), $(LOCALDIRS))
LOCAL_LISPDIRS    := $(addsuffix /$(LISPDIR), $(LOCALDIRS))
LOCAL_REPOS       := $(foreach repo, $(REPOS), $(addsuffix /$(repo), $(LOCAL_LISPDIRS)))

CURRENT_BRANCH = $(shell cd $1; test -d .git && git branch | grep \* | cut -d ' ' -f2)
PULL_JOBS     := $(addprefix .make-pull-, $(REPOS))

##################################################

.PHONY: all test build clean
all: $(DIRS) build
	@$(call ECHO_YELLOW, "make job:all completed!!", "\n", "\n")

test:
	echo $(call CURRENT_BRANCH, site-lisp/leaf.el)

$(DIRS):
	mkdir -p $@

build: $(LOCAL_MIXELCFILES) $(LOCAL_REPOS)

#################### main jobs
##########
$(LOCAL_MIXELCFILES): $(LOCAL_MAKEFILES) $(LOCAL_MIXFILES)
	@$(call ECHO_MAGENTA, "compile $@...", "\n", "")
	$(MAKE) --no-print-directory -C $(@D) .make-elc-$(@F) 2>/dev/null

$(LOCAL_MAKEFILES): $(MAKEFILE)
	ln -s ../../$< $@

$(LOCAL_MIXFILES): $(MIXFILE)
	ln -s ../$(<F) $@

##########
define build_repo
$(addsuffix /$(LISPDIR)/$(1), $(LOCALDIRS)): $(LISPDIR)/$(1)
	@$$(call ECHO_MAGENTA, "compile $$@...", "\n", "")
	mkdir -p $$(@D)
	cp -rf $(LISPDIR)/$$(@F) $$@
	$$(MAKE) --no-print-directory -C $$(dir $$(@D)) .make-repo-$$(@F) 2>/dev/null
endef
$(foreach repo, $(REPOS), $(eval $(call build_repo,$(repo))))

#################### support jobs
##########
$(MIXFILE): $(CONFFILES)
	cat $^ > $(MIXRAWFILE)
	cat $(MIXRAWFILE) | sed -e $(SED_MIXFILE) > $(MIXFILE)
	echo "(provide 'conao-mix)" >> $(MIXFILE)

$(REPODIRS):
	@$(call ECHO_MAGENTA, "git clone $(REMOTE)/$(@F)", "\n", "")
	git clone $(REMOTE)/$(@F) $@

update: $(REPOS:%=.make-update-%) .make-update-.
.make-update-%:
	cd $(LISPDIR)/$*; git pull origin $(call CURRENT_BRANCH, $(LISPDIR)/$*)

clean:
	-rm -rf $(DIRS)
	@$(call ECHO_CYAN, "make job:clean completed!!", "\n", "\n")

# load common my local func
include Makefunc.mk
