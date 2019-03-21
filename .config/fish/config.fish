set SHELL /usr/local/bin/fish

# bindings
function fish_user_key_bindings
  bind \cr 'peco_select_history (commandline -b)'
  bind \c] peco_select_ghq_repository
end

####################
#  paths

# anyenv
set PATH $HOME/.anyenv/bin $PATH
anyenv init - | source

# emacs
set PATH /Applications/Emacs-23.4.app/Contents/MacOS/ $PATH
set PATH /Applications/Emacs-26.1.app/Contents/MacOS/bin $PATH
set PATH $HOME/.cask/bin $PATH

# homebrew
set PATH /usr/local/bin $PATH

# rust
set PATH $HOME/.cargo/bin $PATH

# go
set PATH /usr/local/opt/go/libexec/bin $PATH
set PATH (go env GOPATH)/bin $PATH

# po4a
set PATH $HOME/Develop/git/po4a $PATH
set -g fish_user_paths "/usr/local/opt/texinfo/bin" $fish_user_paths

# ruby gem
# set PATH ~/.anyenv/envs/rbenv/versions/2.5.3/bin $PATH

# flex
set -g fish_user_paths "/usr/local/opt/flex/bin" $fish_user_paths
set -gx LDFLAGS "-L/usr/local/opt/flex/lib"
set -gx CPPFLAGS "-I/usr/local/opt/flex/include"

# bison
set -g fish_user_paths "/usr/local/opt/bison/bin" $fish_user_paths
set -gx LDFLAGS "-L/usr/local/opt/bison/lib"

# travis-ci
set TRAVISTAG packer-1515445631-7dfb2e1

####################
#  aliases
balias g git
alias xmlxpath="xmllint --html --xpath 2>/dev/null"
# alias travis="~/.anyenv/envs/rbenv/versions/2.5.3/lib/ruby/gems/2.5.0/gems/travis-1.8.9/bin/travis"
