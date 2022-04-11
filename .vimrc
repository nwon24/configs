" Begin ~/.vimrc

" Don't be compatible with Vi
set nocompatible
" Be smart about filetypes
filetype plugin on
" syntax highlighing
syntax on

" from sensible.vim:
" 'Allow color schemes to do bright colors without forcing bold.'
if &t_Co == 8 && $TERM !~# '^Eterm'
  set t_Co=16
endif

" Better escape key
imap jk <ESC>

" Relative line numbers
set relativenumber

" Show statusbar
set laststatus=2
" Misc.
set autoindent
set smarttab
set backspace=indent,eol,start

" Searching: incremental and highlight
set incsearch
set hlsearch
nmap <C-x> :noh<CR>
" Ruler - show column and row number
set ruler
" Wildmenu - enhanced completion
set wildmenu

" 'Bubble' text
" These keys are fine because they are synonyms in normal and visual mode.
nmap <C-p> ddkP
nmap <C-n> ddp
vmap <C-p> xkP`[V`]
vmap <C-n> xp`[V`]

" plugins using vim-plug
"
" Automatic installation
let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin(has('nvim')? stdpath('data') . '/plugged' : '~/.vim/plugged')

" A tolerable colorscheme
Plug 'altercation/vim-colors-solarized'
" A handy plugin for exchanging two bits of text
Plug 'tommcdo/vim-exchange'
" Lifesaver for working with code with surroundings
Plug 'tpope/vim-surround'
" Git
Plug 'tpope/vim-fugitive'

call plug#end()

" Colorscheme
set background=dark
colorscheme solarized
call togglebg#map("<F5>")

" End ~/.vimrc
