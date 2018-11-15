set ignorecase
set smartcase
set wrapscan
set incsearch
set inccommand=split

set tabstop=4
set shiftwidth=4
set softtabstop=0
set expandtab
set smarttab
set shiftround

nnoremap <silent> <S-j> :split<CR>
nnoremap <silent> <S-l> :vsplit<CR>
nnoremap <Bar> $:let pos = getpos(".")<CR>:join<CR>:call setpos('.', pos)<CR>

nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk

inoremap <silent> jj <ESC>:<C-u>w<CR>

"dein Scripts-----------------------------
if &compatible
  set nocompatible               " Be iMproved
endif

" Required:
set runtimepath+=/Users/conao/.cache/dein/repos/github.com/Shougo/dein.vim

" Required:
if dein#load_state('/Users/conao/.cache/dein')
  call dein#begin('/Users/conao/.cache/dein')

  " Let dein manage dein
  " Required:
  call dein#add('/Users/conao/.cache/dein/repos/github.com/Shougo/dein.vim')

  " Add or remove your plugins here like this:
  "call dein#add('Shougo/neosnippet.vim')
  "call dein#add('Shougo/neosnippet-snippets')

  " Required:
  call dein#end()
  call dein#save_state()
endif

" Required:
filetype plugin indent on
syntax enable

" If you want to install not installed plugins on startup.
"if dein#check_install()
"  call dein#install()
"endif

"End dein Scripts-------------------------
