EMACS = $(shell echo $(PATH) |\
tr ':' '\0' |\
xargs -0 -I {} find {} -type f 2>/dev/null |\
grep -o "emacs-.*$" | sed -e "s/emacs-//" |\
grep -e "^[0-9]" |\
sort |\
uniq)

all: initfile
	@echo $(EMACS)

initfile:
	@echo $(PATH)
