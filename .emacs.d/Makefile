LOCALDIR           := local
SITE_LISP_DIR      := site-lisp
CONFDIR            := conf
COMBINE            := combine

GITHUB             := https://github.com/conao3

ALL_EMACS_VERS     := $(shell compgen -c emacs- | grep -oP '(?<=emacs-)([0-9]|\.)+' | sort | uniq)

all: mkdir build
	$(warning finish)

mkdir:
	mkdir -p $(LOCALDIR)
	mkdir -p $(SITE_LISP_DIR)

pull:
	git clone $(GITHUB)/leaf.el $(SITE_LISP_DIR)/leaf.el
	git clone $(GITHUB)/orglyth.el $(SITE_LISP_DIR)/orglyth.el

build:
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
