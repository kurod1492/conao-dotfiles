# every time run at login

export EDITOR=emacs

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
