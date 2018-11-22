LOCALDIR           := local
SITE_LISP_DIR      := site-lisp
CONFDIR            := conf
COMBINE            := combine

GITHUB             := https://github.com/conao3
REPOS              := leaf.el orglyth.el cort.el

DUMMY_FILE_DIR     := .make
PULL               := $(DUMMY_FILE_DIR)/pull
DEPLOY             := $(DUMMY_FILE_DIR)/deploy
INIT               := $(PULL) $(DEPLOY)

ALL_EMACS_VERS     := $(shell compgen -c emacs- | grep -oP '(?<=emacs-)([0-9]|\.)+' | sort | uniq)

all: mkdir $(INIT) build
	$(call ECHO_YELLOW, "make all is finish")

mkdir:
	mkdir -p $(SITE_LISP_DIR)
	mkdir -p $(LOCALDIR)
	mkdir -p $(DUMMY_FILE_DIR)
	cd $(LOCALDIR); mkdir -p $(ALL_EMACS_VERS)

$(PULL):
	-@for repo in $(REPOS); do \
	  echo "\n"; \
	  $(COLOR_MAGENTA); \
	  echo "===  git clone $(GITHUB)/$${repo}  ==="; \
	  $(COLOR_DEFAULT); \
	  git clone $(GITHUB)/$${repo} $(SITE_LISP_DIR)/$${repo}; \
	done
	@echo "\n"
	$(call ECHO_CYAN, "clone completed!!")
	@echo "\n"
	@touch $(PULL)

$(DEPLOY):
	find local -type d -depth 1 | sed 's/$$/\/Makefile/g' | \
	  xargs -n1 cp Makefile-local.mk
	find local -type d -depth 1 | sed 's/$$/\//g' | \
	  xargs -n1 cp -rf $(SITE_LISP_DIR)
	@touch $(DEPLOY)

build:
# target Emacs(s)
	$(info Emacs versions... $(ALL_EMACS_VERS))

# combine conf/*.el files
	find $(CONFDIR) -type f -name "*.el"| xargs cat > $(LOCALDIR)/$(COMBINE).el
	sed -e 's/;.*//g' -e 's/(provide .*)//g' -e '/^$$/d' -i $(LOCALDIR)/$(COMBINE).el
	echo "(provide 'combine)" >> $(LOCALDIR)/$(COMBINE).el

# byte compile by all Emacs(s)
	@cd $(LOCALDIR); \
	for ver in $(ALL_EMACS_VERS); do \
		echo \\n; \
		$(COLOR_MAGENTA); \
		echo "===  compile by emacs-$${ver}...  ==="; \
		$(COLOR_DEFAULT); \
		emacs-$${ver} -batch -f batch-byte-compile $(COMBINE).el; \
		mkdir -p $${ver}; \
		mv $(COMBINE).elc $${ver}/; \
	done
	@echo "\n"
	$(call ECHO_CYAN, "compile completed!!")
	@echo "\n"

clean:
	-rm -rf $(LOCALDIR)
	-rm -rf $(DUMMY_FILE_DIR)

# load common my local func
include Makefunc.mk
