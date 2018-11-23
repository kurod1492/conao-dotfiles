MAKEDIR     := .make
LOCALDIR    := local
LISPDIR     := site-lisp
CONFDIR     := conf

MIXRAW      := mixed-raw
MIX         := mixed
MIXRAWFILE  := $(LOCALDIR)/$(MIXRAW).el
MIXFILE     := $(LOCALDIR)/$(MIX).el

GITHUB      := https://github.com
REMOTE      := $(GITHUB)/conao3
REPOS       := leaf.el orglyth.el cort.el

MKDIR       := $(MAKEDIR)/mkdir
CLONE       := $(MAKEDIR)/clone
DEPLOY      := $(MAKEDIR)/deploy
INIT        := $(MKDIR) $(CLONE) $(DEPLOY)

EMACS_RAW   := $(filter-out emacs-undumped, $(shell compgen -c emacs- | xargs))
ALL_EMACS   := $(sort $(EMACS_RAW) $(EMACS_RAW))  # delete duplicate element
EMACS_VERS  := $(subst emacs-,,$(ALL_EMACS))

LOCALDIRS   := $(addprefix $(LOCALDIR)/,$(EMACS_VERS))
CREATEDIRS  := $(MAKEDIR) $(LOCALDIR) $(LISPDIR) $(LOCALDIRS)


.PHONY: all test build clean
all: $(INIT) build
	@$(call ECHO_YELLOW, "make job:all completed!!", "\n", "\n")

test:
	echo "$(CREATE_DIRS)"
	echo "$(EMACS_VERS)"
	echo $(addprefix $(LOCALDIR)/,$(EMACS_VERS))
	$(foreach ver, $(EMACS_VERS), echo "$(ver)";)
	$(foreach local, $(LOCALDIRS), echo "$(local)";)
	@$(foreach local, $(LOCALDIRS), \
	  echo "ln -s ../../Makefile-local.mk $(local)/Makefile"; \
	  echo "cp -rf $(LISPDIR) $(local)/"; \
	  $(MAKE) -C $(local); \
	)

$(MKDIR):
	mkdir -p $(CREATEDIRS)
	@$(call ECHO_CYAN, "make job:mkdir completed!!", "\n", "\n")
	@touch $(MKDIR)

$(CLONE):
	@$(foreach repo, $(REPOS), \
	  $(call ECHO_MAGENTA, "git clone $(REMOTE)/$(repo)", "\n", ""); \
	  git clone $(REMOTE)/${repo} $(LISPDIR)/${repo}; \
	)
	@$(call ECHO_CYAN, "make job:clone completed!!", "\n", "\n")
	@touch $(CLONE)

$(DEPLOY):
# mix conf/*.el files
	find $(CONFDIR) -type f -name "*.el"| xargs cat > $(MIXRAWFILE)
	cat $(MIXRAWFILE) | \
	  sed -e 's/;.*//g' -e 's/(provide .*)//g' -e '/^$$/d' > $(MIXFILE)
	echo "(provide 'conao-mix)" >> $(MIXFILE)

# deploy files
	$(foreach local, $(LOCALDIRS), \
	  ln -s ../../Makefile-local.mk $(local)/Makefile; \
	  cp -rf $(LISPDIR) $(local)/; \
	  cp $(MIXFILE) $(local)/; \
	)
	@$(call ECHO_CYAN, "make job:deploy completed!!", "\n", "\n")
	@touch $(DEPLOY)

build:
# target Emacs(s)
	@$(call ECHO_YELLOW, "Emacs versions... $(EMACS_VERS)", "", "")
	@$(foreach local, $(LOCALDIRS), \
	  $(call ECHO_MAGENTA, "compile on $(local)...", "\n", ""); \
	  $(MAKE) -C $(local); \
	)
	@$(call ECHO_CYAN, "make job:build completed!!", "\n", "\n")

clean:
	-rm -rf $(CREATEDIRS)
	@$(call ECHO_CYAN, "make job:clean completed!!", "\n", "\n")

# load common my local func
include Makefunc.mk
