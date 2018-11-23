MAKEDIR    := .make
LOCALDIR   := local
LISPDIR    := site-lisp
CONFDIR    := conf

COMBINE    := combine

GITHUB     := https://github.com/conao3
REPOS      := leaf.el orglyth.el cort.el

MKDIR      := $(MAKEDIR)/mkdir
PULL       := $(MAKEDIR)/pull
DEPLOY     := $(MAKEDIR)/deploy
INIT       := $(MKDIR) $(PULL) $(DEPLOY)

EMACS_RAW  := $(shell compgen -c emacs- | xargs)
ALL_EMACS  := $(sort $(EMACS_RAW) $(EMACS_RAW))
EMACS_VERS := $(subst emacs-,,$(ALL_EMACS))


all: $(INIT) build
	@$(call ECHO_YELLOW, "make all is finish", "", "\n")

test:
	echo $(EMACS_VERS)
	echo $(addprefix $(LOCALDIR)/,$(EMACS_VERS))

$(MKDIR):
	mkdir -p $(LISPDIR) $(LOCALDIR) $(MAKEDIR)

	@touch $(MKDIR)

$(PULL):
	@$(foreach repo,$(REPOS), \
	  $(call ECHO_MAGENTA, "git clone $(GITHUB)/${repo}", "\n", ""); \
	  git clone $(GITHUB)/${repo} $(LISPDIR)/${repo}; \
	)
	@$(call ECHO_CYAN, "clone completed!!", "\n", "\n")
	@touch $(PULL)

$(DEPLOY):
	find local -type d -depth 1 | sed 's/$$/\/Makefile/g' | \
	  xargs -n1 cp Makefile-local.mk
	find local -type d -depth 1 | sed 's/$$/\//g' | \
	  xargs -n1 cp -rf $(LISPDIR)
	@touch $(DEPLOY)

build:
# target Emacs(s)
	@$(call ECHO_YELLOW, "Emacs versions... $(EMACS_VERS)", "", "")

# combine conf/*.el files
	find $(CONFDIR) -type f -name "*.el"| xargs cat > $(LOCALDIR)/$(COMBINE).el
	sed -e 's/;.*//g' -e 's/(provide .*)//g' -e '/^$$/d' -i $(LOCALDIR)/$(COMBINE).el
	echo "(provide 'combine)" >> $(LOCALDIR)/$(COMBINE).el

# byte compile by all Emacs(s)
	@cd $(LOCALDIR); \
	@$(foreach ver, $(EMACS_VERS), \
	  $(call ECHO_MAGENTA, "compile by emacs-${ver}...", "\n", ""); \
	  emacs-${ver} -batch -f batch-byte-compile $(COMBINE).el; \
	  mkdir -p ${ver}; \
	  mv $(COMBINE).elc ${ver}/; \
	)
	@$(call ECHO_CYAN, "compile completed!!", "\n", "\n")

clean:
	-rm -rf $(LOCALDIR)
	-rm -rf $(LISPDIR)
	-rm -rf $(MAKEDIR)

# load common my local func
include Makefunc.mk
