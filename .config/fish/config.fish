set SHELL /Users/conao/local/homebrew/bin/fish

# anyenv
set PATH $HOME/.anyenv/bin $PATH
anyenv init - | source

# emacs
if test -d /Applications/Emacs-25.app
  set PATH /Applications/Emacs-25.app/Contents/MacOS/bin/ $PATH
end

if test -d /Applications/Emacs-25.3.app/
   set PATH /Applications/Emacs-25.3.app/Contents/MacOS/bin $PATH
end

# homebrew
set PATH /usr/local/bin $PATH

## llvm
if test -d "/Users/conao/local/homebrew/opt/llvm/bin"
   set -g fish_user_paths "/Users/conao/local/homebrew/opt/llvm/bin" $fish_user_paths
   set -g fish_user_paths "/Users/conao/local/homebrew/opt/openssl/bin" $fish_user_paths
end
if test -d "/usr/local/opt/llvm/bin"
   set -g fish_user_paths "/usr/local/opt/llvm/bin" $fish_user_paths
end

# bindings
function fish_user_key_bindings
  bind \cr 'peco_select_history (commandline -b)'
  bind \c] peco_select_ghq_repository
end

# aliases
balias g git
