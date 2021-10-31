set nocompatible

let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')

Plug 'preservim/nerdtree'
Plug 'altercation/vim-colors-solarized'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-commentary'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'

call plug#end()

syntax on
set background=dark
colorscheme solarized

filetype plugin indent on
set tabstop=8
set softtabstop=8
set expandtab
set shiftwidth=8
set autoindent

set showcmd
set backspace=eol,indent,start

set incsearch
set hlsearch

set relativenumber

set path+=**
set wildmenu

set laststatus=2

imap jj <ESC>

autocmd BufReadPost *
  \ if line ("'\"") > 1 && line ("'\"") <= line("$") |
  \   exe "normal! g`\"" |
  \ endif
