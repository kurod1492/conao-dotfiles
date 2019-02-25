# This program is free software: you can redistribute it and/or modify it
# under the terms of the Affero GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or (at your
# option) any later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the Affero GNU General Public
# License for more details.
#
# You should have received a copy of the Affero GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

set --local secret_env_vars_path ~/.config/fish/secret_env_vars.fish
test -e $secret_env_vars_path && source $secret_env_vars_path

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

# tabtab source for serverless package
# uninstall by removing these lines or running `tabtab uninstall serverless`
[ -f /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.fish ]; and . /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.fish
# tabtab source for sls package
# uninstall by removing these lines or running `tabtab uninstall sls`
[ -f /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.fish ]; and . /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.fish
# tabtab source for slss package
# uninstall by removing these lines or running `tabtab uninstall slss`
[ -f /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/slss.fish ]; and . /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/slss.fish