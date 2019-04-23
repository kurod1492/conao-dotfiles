# .bash_profile

# this file used for ...
#     environment variables

# get the aliases and functions
# eval .bashrc if exist (and readable)
test -r ~/.bashrc && . ~/.bashrc

# user specific environment and startup programs
export PATH=$HOME/.local/bin:$HOME/bin:$PATH

# bash history settings
export HISTSIZE=10000
export HISTFILESIZE=10000
export HISTCONTROL="ignoredups"

# default editor
emacs --version > /dev/null 2>&1
if [ $? = 0 ]; then
    export EDITOR=emacs
fi
