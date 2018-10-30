set SHELL /usr/local/bin/fish

# anyenv
set PATH $HOME/.anyenv/bin $PATH
anyenv init - | source

# emacs
set PATH /Applications/Emacs-23.4.app/Contents/MacOS/ $PATH
set PATH /Applications/Emacs-26.1.app/Contents/MacOS/bin $PATH

# homebrew
set PATH /usr/local/bin $PATH

# bindings
function fish_user_key_bindings
  bind \cr 'peco_select_history (commandline -b)'
  bind \c] peco_select_ghq_repository
end

# rust
set PATH $HOME/.cargo/bin $PATH

# go
set PATH /usr/local/opt/go/libexec/bin $PATH
set PATH (go env GOPATH)/bin $PATH

# aliases
balias g git
alias xmlxpath="xmllint --html --xpath 2>/dev/null"