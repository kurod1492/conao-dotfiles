# every time run at login

export EDITOR=emacs

###
### alias
###
# list directory with num for cd
function cdl {
    local -a dirlist opt_f=false
    local i d num=0 dirnum opt opt_f
    while getopts ":f" opt ; do
        case $opt in
            f ) opt_f=true ;;
        esac
    done
    shift $(( OPTIND -1 ))
    dirlist[0]=..
    for d in * ; do test -d "$d" && dirlist[$((++num))]="$d" ; done
    for i in $( seq 0 $num ) ; do printf "%3d %s%b\n" $i "$( $opt_f && echo -n "$PWD/" )${dirlist[$i]}" ; done
    read -p "select number: " dirnum
    if [ -z "$dirnum" ] ; then
        echo "$FUNCNAME: Abort." 1>&2
    elif ( echo $dirnum | egrep '^[[:digit:]]+$' > /dev/null ) ; then
        cd "${dirlist[$dirnum]}"
    else
        echo "$FUNCNAME: Something wrong." 1>&2
    fi
}

## highlight ls
case "$(uname)$" in
    'Darwin'|'FreeBSD') alias ls='ls -aG'
                        ;;
    *)                  alias ls='ls -a --color=auto'
esac

alias la='ls'
alias ll='ls -l'

## prompt overwrite files
alias mv='mv -i'
alias rm='rm -i'
alias cp='cp -i'

## make tree directory (mkdir tmp/tmp2/tmp3)
## verbose make directory
alias mkdir='mkdir -pv'

## ping 5 times
alias ping='ping -c 5'

## history latest 10 entry
alias h='history 10'

## symple redef
alias p="pwd"

###
### git
###
git config --global user.name 'conao3'
git config --global user.email 'conao3@gmail.com'
git config --global core.editor emacs
git config --global core.quotepath false
git config --global core.precomposeunicode true
git config --global color.ui auto

## alias
git config --global alias.ch checkout

###
### depending systems
###
if [ "$(uname)" = 'Darwin' ]; then
    # mac
    function cdf {
	    target=`osascript -e 'tell application "Finder" to if (count of Finder windows) > 0 then get POSIX path of (target of front Finder window as text)'`
	    if [ "$target" != "" ]; then
		    cd "$target"; pwd
	    else
		    echo 'No Finder window found' >&2
	    fi
    }
    alias f='open .'

    # quicklook
    alias ql='qlmanage -p "$@" >& /dev/null'

    export PYENV_ROOT=/usr/local/var/pyenv
    export PATH="/Users/conao/local/homebrew/bin:$PATH"
    export PATH="/Users/conao/local/homebrew/sbin:$PATH"
    export PATH="/usr/local/var/pyenv/shis/python:$PATH"
    export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
    export PATH="$PATH:/Applications/microchip/xc8/v1.38/bin"
    export PATH="$PATH:/Applications/microchip/xc16/v1.26/bin"
    
    alias brew="PATH=\
/Users/conao/local/homebrew/bin:\
/Users/conao/local/homebrew/sbin:\
/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin brew"
    if which pyenv > /dev/null; then eval "$(pyenv init -)"; fi

    if [ -e /Users/conao/local/homebrew/bin/dot ]; then
        export GRAPHVIZ_DOT=/Users/conao/local/homebrew/bin/dot
    fi
else
	# Linux etc.
    echo 'linux'
fi

# every time run command
ll
