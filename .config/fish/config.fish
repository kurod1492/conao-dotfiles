set SHELL /Users/conao/local/homebrew/bin/fish

# anyenv
set PATH $HOME/.anyenv/bin $PATH
anyenv init - | source

# bindings
function fish_user_key_bindings
  bind \cr 'peco_select_history (commandline -b)'
  bind \c] peco_select_ghq_repository
end