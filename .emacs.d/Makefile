LOCALDIR           := local
SITE_LISP_DIR      := site-lisp
CONFDIR            := conf
COMBINE            := combine

GITHUB             := https://github.com/conao3
REPOS              := leaf.el orglyth.el
PULL               := pull
ALL_EMACS_VERS     := $(shell compgen -c emacs- | grep -oP '(?<=emacs-)([0-9]|\.)+' | sort | uniq)

all: pull build
	$(warning finish)

$(PULL):
	mkdir -p $(SITE_LISP_DIR)
	for repo in $(REPOS); do \
	  git clone $(GITHUB)/$${repo} $(SITE_LISP_DIR)/$${repo} >> $(PULL); \
	done

build:
# make working directory
	mkdir -p $(LOCALDIR)

# target Emacs(s)
	$(info Emacs versions... $(ALL_EMACS_VERS))

# combine conf/*.el files
	find $(CONFDIR) -type f -name "*.el"| xargs cat > $(LOCALDIR)/$(COMBINE).el
	sed -e 's/;.*//g' -e 's/(provide .*)//g' -e '/^$$/d' -i $(LOCALDIR)/$(COMBINE).el

# byte compile by all Emacs(s)
	cd $(LOCALDIR); \
	for ver in $(ALL_EMACS_VERS); do \
		echo \\n; \
		echo compile by emacs-$$ver...; \
		emacs-$$ver -batch -f batch-byte-compile $(COMBINE).el; \
		mkdir -p $$ver; \
		mv $(COMBINE).elc $$ver/; \
	done

clean:
	-rm -rf $(LOCALDIR)
	-rm -rf $(SITE_LISP_DIR)
	-rm $(PULL)

# load common my local func
include Makefunk.mk
