"" --------------------------------------------------------------------------------
"" Dustin's VIM configuration
"" --------------------------------------------------------------------------------
"" Color scheme colorscheme lucius
colorscheme lucius
LuciusDark

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

"highlight clear SignColumn

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

"" Local keys and such
let mapleader=","
let maplocalleader=" "

"" File-type highlighting and configuration.
syntax on
filetype on
filetype plugin on
filetype indent on

"" Paste from clipboard
nnoremap <Leader>, "+gP

"" Copy from clipboard
xnoremap <Leader>. "+y 

"" Move cursor by display lines when wrapping
nnoremap j gj
nnoremap k gk

"" Map CTRL-TAB to move through tabbed windows (like a browser)
noremap <c-tab> :tabnext<cr>
noremap <c-A-tab> :tabnext<cr>

"" Map leader-q to quit out of window
nnoremap <leader>q :q<cr>

"" Move around split
"nnoremap <C-h> <C-w>h
"nnoremap <C-j> <C-w>j
"nnoremap <C-k> <C-w>k
"nnoremap <C-l> <C-w>l

"" Easier to yank entire line
nnoremap Y y$

"" Edit and source vimrc file right from within vim
nnoremap <silent> <Leader>gv :tabnew<CR>:e ~/dotfiles/.vimrc<CR>
nnoremap <silent> <Leader>sv :so ~/dotfiles/.vimrc<CR>

"" Move buffers
nnoremap <Tab> :bnext<cr> 

"" Clears search buffer so highlighting is gone
nmap <silent> ,/ :nohlsearch<CR>

"" Like a boss, sudo AFTER opening the file to write
cmap w!! w !sudo tee % >/dev/null

"" Change cursor on mode
:autocmd InsertEnter * set cul
:autocmd InsertLeave * set nocul

"" Set gui options to hide extra shit we don't need
:set guioptions-=m  "remove menu bar
:set guioptions-=T  "remove toolbar
:set guioptions-=r  "remove right-hand scroll bar
:set guioptions-=L  "remove left-hand scroll bar

if has('gui_running')
  set guifont=Hack
endif

"" --------------------------------------------------------------------------------
"" Development plugins
"" --------------------------------------------------------------------------------
"" call plug#begin('~/.vim/plugged')

" Source COC configs
source $HOME/.config/nvim/plug-config/coc.vim

call plug#begin('~/.config/nvim/plugged')

"" COC aka code completion
Plug 'neoclide/coc.nvim', {'branch': 'release'}

"" Fuzzy search
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
nnoremap <leader>f :Files<CR>
nnoremap <localleader>f :enew<CR>

"" Buffer bar at the top of screen
Plug 'ap/vim-buftabline'

"" Todotxt
Plug 'freitass/todo.txt-vim'

"" Cool start screen
Plug 'mhinz/vim-startify'
nnoremap <leader>s :SSave<CR>
nnoremap <leader>c :SClose<CR>
nnoremap <leader>d :SDelete<CR>

let g:startify_lists = [
          \ { 'type': 'dir',       'header': ['   Current Directory '. getcwd()] },
          \ { 'type': 'sessions',  'header': ['   Sessions']       },
          \ { 'type': 'bookmarks', 'header': ['   Bookmarks']      }
          \ ]

let g:startify_bookmarks = [
            \ '~/.config/nvim/init.vim',
            \ '~/.zshrc',
            \ '~/code',
            \ ]

"" Color hex codes
Plug 'chrisbra/Colorizer'

"" Status bar
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
let g:airline_theme='bubblegum'
let g:airline_powerline_fonts = 1

"" Elixir specific
Plug 'elixir-editors/vim-elixir'

"" Jump around
Plug 'justinmk/vim-sneak'

"" Tmux + Vim love
Plug 'christoomey/vim-tmux-navigator'

call plug#end()
