# .bashrc

# this file used for ...
#     shell variables
#     define alias
#     define shell function
#     set command line complication

# eval global definitions if exists (and readable)
test -r /etc/bashrc && . /etc/bashrc

# edit terminal disp
PS1="\h:\w \u\$ "

# C-s to isearch
stty stop undef

# save history immidiatly
function share_history {
    history -a
    history -c
    history -r
}
PROMPT_COMMAND='share_history'
shopt -u histappend

function exists {
    type $1 >/dev/null 2>&1; return $?;
}

# aliases
if exists colordiff; then
    alias diff="colordiff -n"
fi

if exists emacsclient; then
    alias ec="emacsclient"
fi

if exists hub; then
    alias git="hub"
fi

## highlight ls
case "$(uname)" in
    'Darwin'|'FreeBSD') alias ls='ls -aG'
                        ;;
    *)                  alias ls='ls -a --color=auto'
esac

## ls after cd
function cdls {
    \cd "$@" && ls
}
alias cd="cdls"

## history latest 10 entry
alias h='history 10'

## depend on systems
if [ "$(uname)" = 'Darwin' ]; then
    echo 'mac'
else
    echo 'linux'
fi

# every time run command
ll
