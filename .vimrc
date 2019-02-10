"" --------------------------------------------------------------------------------
"" VIM configuration
"" --------------------------------------------------------------------------------
"" Dustin Lyons

"" Color scheme
colorscheme lucius 

"" General
set number
set history=1000
set nocompatible
set modelines=0
set encoding=utf-8
set scrolloff=3
set showmode
set showcmd
set hidden
set wildmenu
set wildmode=list:longest
set cursorline
set ttyfast
set nowrap
set ruler
set backspace=indent,eol,start
set laststatus=2

" Dir stuff
set nobackup
set nowritebackup
set noswapfile

" Relative line numbers for easy movement
set relativenumber
set rnu

"" Whitespace
set tabstop=8
set shiftwidth=4
set softtabstop=4
set expandtab	

"" Searching
set incsearch		
set gdefault

"" Statusbar
set nocompatible " Disable vi-compatibility
set laststatus=2 " Always show the statusline

"" Use comma as <Leader> key instead of backslash
let mapleader=","

"" File-type highlighting and configuration.
syntax on
filetype on
filetype plugin on
filetype indent on

"" Paste from clipboard
nnoremap <Leader>, "+gP

"" Move cursor by display lines when wrapping
nnoremap j gj
nnoremap k gk

"" Map CTRL-TAB to move through tabbed windows (like a browser)
noremap <c-tab> :tabnext<cr>
noremap <c-A-tab> :tabnext<cr>

"" Map leader-q to quit out of window
nnoremap <leader>q :q<cr>

"" Move around split
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

"" Edit and source vimrc file right from within vim
nnoremap <silent> <Leader>gv :tabnew<CR>:e ~/dotfiles/.vimrc<CR>
nnoremap <silent> <Leader>sv :so ~/dotfiles/.vimrc<CR>

"" Move buffers
nnoremap <C-TAB> :bnext<cr> 
nnoremap <A-TAB> :bprev<cr> 

"" Clears search buffer so highlighting is gone
nmap <silent> ,/ :nohlsearch<CR>

"" Like a boss, sudo AFTER opening the file to write
cmap w!! w !sudo tee % >/dev/null

"" --------------------------------------------------------------------------------
"" Development plugins
"" --------------------------------------------------------------------------------
call plug#begin('~/.vim/plugged')

"" Fuzzy search
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
map ; :Files<CR>

"" Cool start screen
Plug 'mhinz/vim-startify'

"" Vim diff lines
Plug 'airblade/vim-gitgutter'

"" Vim prettify JS
Plug 'prettier/vim-prettier'

"" ES6 linter, Typescript syntax
Plug 'w0rp/ale'
Plug 'leafgarland/typescript-vim' 

call plug#end()

"" Auto format and run linter
