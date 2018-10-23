LOCALDIR           := local
CONFDIR            := conf
COMBINE            := combine
ALL_EMACS_VERS     := $(shell compgen -c emacs- | grep -oP '(?<=emacs-)([0-9]|\.)+' | sort | uniq)

all: build
	$(warning finish)

build:
# target Emacs(s)
	$(info Emacs versions... $(ALL_EMACS_VERS))

# create build directory
	mkdir -p $(LOCALDIR)

# combine conf/*.el files
	find $(CONFDIR) -type f -name "*.el"| xargs cat > $(LOCALDIR)/$(COMBINE).el
	sed -e 's/;.*//g' -e 's/(provide .*)//g' -e '/^$$/d' -i $(LOCALDIR)/$(COMBINE).el

# byte compile by all Emacs(s)
	cd $(LOCALDIR); \
	for ver in $(ALL_EMACS_VERS); do \
		echo \\n; \
		echo compile by emacs-$$ver...; \
		emacs-$$ver -batch -f batch-byte-compile $(COMBINE).el; \
		mv $(COMBINE).elc $(COMBINE)-$$ver.elc; \
	done

clean:
	-rm -rf $(LOCALDIR)
