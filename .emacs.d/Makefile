LOCALDIR           := local
SITE_LISP_DIR      := site-lisp
CONFDIR            := conf
COMBINE            := combine

GITHUB             := https://github.com/conao3
REPOS              := leaf.el orglyth.el cort.el

DUMMY_FILE_DIR     := .make
MKDIR              := $(DUMMY_FILE_DIR)/mkdir
PULL               := $(DUMMY_FILE_DIR)/pull
DEPLOY             := $(DUMMY_FILE_DIR)/deploy
INIT               := $(MKDIR) $(PULL) $(DEPLOY)

ALL_EMACS_VERS     := $(shell compgen -c emacs- | grep -oP '(?<=emacs-)([0-9]|\.)+' | sort | uniq)

all: $(INIT) build
	@$(call ECHO_YELLOW, "make all is finish", "", "\n")

$(MKDIR):
	mkdir -p $(SITE_LISP_DIR)
	mkdir -p $(LOCALDIR)
	mkdir -p $(DUMMY_FILE_DIR)
	cd $(LOCALDIR); mkdir -p $(ALL_EMACS_VERS)
	@touch $(MKDIR)

$(PULL):
	@$(foreach repo,$(REPOS), \
	  $(call ECHO_MAGENTA, "git clone $(GITHUB)/${repo}", "\n", ""); \
	  git clone $(GITHUB)/${repo} $(SITE_LISP_DIR)/${repo}; \
	)
	@$(call ECHO_CYAN, "clone completed!!", "\n", "\n")
	@touch $(PULL)

$(DEPLOY):
	find local -type d -depth 1 | sed 's/$$/\/Makefile/g' | \
	  xargs -n1 cp Makefile-local.mk
	find local -type d -depth 1 | sed 's/$$/\//g' | \
	  xargs -n1 cp -rf $(SITE_LISP_DIR)
	@touch $(DEPLOY)

build:
# target Emacs(s)
	@$(call ECHO_YELLOW, "Emacs versions... $(ALL_EMACS_VERS)", "", "")

# combine conf/*.el files
	find $(CONFDIR) -type f -name "*.el"| xargs cat > $(LOCALDIR)/$(COMBINE).el
	sed -e 's/;.*//g' -e 's/(provide .*)//g' -e '/^$$/d' -i $(LOCALDIR)/$(COMBINE).el
	echo "(provide 'combine)" >> $(LOCALDIR)/$(COMBINE).el

# byte compile by all Emacs(s)
	@cd $(LOCALDIR); \
	@$(foreach ver, $(ALL_EMACS_VERS), \
	  $(call ECHO_MAGENTA, "compile by emacs-${ver}...", "\n", ""); \
	  emacs-${ver} -batch -f batch-byte-compile $(COMBINE).el; \
	  mkdir -p ${ver}; \
	  mv $(COMBINE).elc ${ver}/; \
	)
	@$(call ECHO_CYAN, "compile completed!!", "\n", "\n")

clean:
	-rm -rf $(LOCALDIR)
	-rm -rf $(SITE_LISP_DIR)
	-rm -rf $(DUMMY_FILE_DIR)

# load common my local func
include Makefunc.mk
