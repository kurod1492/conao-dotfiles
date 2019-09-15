all:

include Makefunc.mk

DEV_PKGS := leaf.el leaf-keywords.el orglyth.el cort-test.el seml-mode.el navbar.el
DEV_PKGS  += feather.el feather-server.el leaf-browser.el solarized-emacs
DEV_PKGS  += liskk.el
DEV_PKGS  += phantom-inline-comment annotate.el point-history
DEV_PKGS  += ivy-posframe

CLONE_URLS :=

EMACS_LST := 22.0 22.1 22.2 22.3
EMACS_LST  += 23.0 23.1 23.2 23.3 23.4
EMACS_LST  += 24.0 24.1 24.2 24.3 24.4 24.5
EMACS_LST  += 25.0 25.1 25.2 25.3
EMACS_LST  += 26.0 26.1 26.2 26.3
EMACS_LST  += 27.0
DIRS := local $(EMACS_LST:%=local/%)

# xargs option
XARGS := xargs -t $(shell if xargs -r > /dev/null 2>&1; then echo "-r"; else echo ""; fi)
P ?= 8

##################################################

.PHONY: all test build clean
.PRECIOUS: site-lisp/%
all: $(DIRS) build
	@$(call ECHO_YELLOW,"make job:all completed!!","\n","\n")

$(DIRS):
	mkdir -p $@

build: $(EMACS_LST:%=local/%/conf.el) $(EMACS_LST:%=local/%/site-lisp) $(EMACS_LST:%=local/%/latex-math-preview-cache) $(EMACS_LST:%=local/%/snippets) $(EMACS_LST:%=local/%/templete)

##############################

local/%/conf.el: conf.el
	cp $< $@
	-type emacs-$* > /dev/null 2>&1 && \
	  cd $(@D); emacs-$* -Q --batch -f batch-byte-compile $(@F)

####################

local/%/latex-math-preview-cache:
	ln -s ../../$(@F) $@
local/%/snippets:
	ln -s ../../$(@F) $@
local/%/templete:
	ln -s ../../$(@F) $@

####################

local/%/site-lisp: site-lisp $(DEV_PKGS:%=site-lisp/%)
	cp -rf site-lisp $@
	find $@ -name '*.elc' | $(XARGS) rm -rf
	-type emacs-$* > /dev/null 2>&1 && \
	  find $@ -maxdepth 1 -type d | \
	    $(XARGS) -n1 -I%% bash -c \
	      "cd %%; [ -e Makefile ] && make EMACS=emacs-$*"

site-lisp/%:
	if [ -d ~/dev/repos/$* ]; then \
	  ln -s ~/dev/repos/$* $@; \
	else \
	  git clone https://github.com/conao3/$*.git $@; \
	fi

site-lisp:
	mkdir $@
	cd $@; echo $(CLONE_URLS) | $(XARGS) -n1 git clone

##############################

update:
	git pull origin $$(git symbolic-ref --short HEAD)
	-find site-lisp -maxdepth 1 -type d | \
	  $(XARGS) -n1 -P$(P) -I%% bash -c \
	    "cd %%; git pull origin \$$(git symbolic-ref --short HEAD)"
	find local -maxdepth 2 -type d -name site-lisp | $(XARGS) rm -rf
	$(MAKE)

unshallow:
	-find site-lisp -maxdepth 1 -type d | \
	  $(XARGS) -n1 -P$(P) -I%% bash -c \
	    "cd %%; git fetch --unshallow"
	find local -maxdepth 2 -type d -name site-lisp | $(XARGS) rm -rf
	$(MAKE)

clean:
	-rm -rf local
	@$(call ECHO_CYAN,"make job:clean completed!!","\n","\n")
