all:

include Makefunc.mk

DEV_PKGS := leaf.el orglyth.el cort-test.el seml-mode.el po-mode.el navbar.el
DEV_PKGS  += feather.el feather-server.el leaf-browser.el

CLONE_URLS :=

EMACS_LST := 22.0 22.1 22.2 22.3
EMACS_LST  += 23.0 23.1 23.2 23.3 23.4
EMACS_LST  += 24.0 24.1 24.2 24.3 24.4 24.5
EMACS_LST  += 25.0 25.1 25.2 25.3
EMACS_LST  += 26.0 26.1 26.2
EMACS_LST  += 27.0
DIRS := site-lisp local $(EMACS_LST:%=local/%/site-lisp)

# xargs parallel option
P ?= 8

##################################################

.PHONY: all test build clean
all: $(DIRS) build
	@$(call ECHO_YELLOW,"make job:all completed!!","\n","\n")

$(DIRS):
	mkdir -p $@

build: $(EMACS_LST:%=local/%/site-lisp) $(EMACS_LST:%=local/%/latex-math-preview-cache)

##############################

local/%/latex-math-preview-cache:
	ln -s ../../latex-math-preview-cache $@

local/%/site-lisp: $(DEV_PKGS:%=site-lisp/%)
	cp -rf site-lisp $@
	find $@ -name '*.elc' | xargs rm -rf
	-type emacs-$* > /dev/null && \
	  find $@ -depth 1 -type d |
	    xargs bash -c -I^ \
	      "cd ^; [ -e Makefile ] && make EMACS=emacs-$*"

site-lisp/%:
	if [ -d ~/dev/repos/$* ]; then \
	  ln -s ~/dev/repos/$* $@ \
	else \
	  git clone https://github.com/conao3/dotfiles.git $@ \
	fi

##############################

update:
	git pull origin $$(git symbolic-ref --short HEAD)
	-find site-lisp -depth 1 -type d | \
	  xargs -n1 -P$(P) -t -I^ bash -c \
	    "cd ^; git pull origin \$$(git symbolic-ref --short HEAD)"

unshallow:
	-find site-lisp -depth 1 -type d | \
	  xargs -n1 -P$(P) -t -I^ bash -c \
	    "cd ^; git fetch --unshallow"

clean:
	-rm -rf local
	@$(call ECHO_CYAN,"make job:clean completed!!","\n","\n")
