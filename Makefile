MAKEDIRS := .emacs.d
dotfiles := .emacs.d .gitignore

.PHONY: all
all:
	$(MAKE) -C .emacs.d
	ln -s .dotfiles/.emacs.d ~/
	ln -s .dotfiles/.gitignore ~/
