" Vim Settings
filetype off                            " required 
set nocompatible                        " be iMproved, required 
syntax on				" enable syntax highlighting
set t_Co=256				" enable 256 colors, true colors
set ruler               		" enable line and column number of the cursor on right side of statusline
set number relativenumber		" enable “Hybrid” line numbers
set path+=**                            " enable  searche current directory recursively.
set incsearch 				" enable incremental search 
set ignorecase                          " enable case-insensitive searching
set hidden                              " enable multiple buffers open
set showmatch           		" highlight matching parentheses / brackets [{()}]
set cursorline				" highlight the current line
set cursorcolumn			" highlight the current column
set wildmenu                            " Display all matches when tab complete.
set nobackup                            " No auto backups
set noswapfile                          " No swap

" Load plugins 
call plug#begin()
Plug 'scrooloose/nerdtree'              " Nerdtree
Plug 'preservim/nerdcommenter'          " Comment functions
Plug 'ryanoasis/vim-devicons'           " Icons for Nerdtree
Plug 'ap/vim-css-color'                 " Color previews for CSS
Plug 'junegunn/vim-emoji'               " Vim needs emojis!
Plug 'itchyny/lightline.vim'            " statusline/tabline plugin for Vim
Plug 'vim-python/python-syntax'         " Python highlighting
call plug#end()

" Theme
" put colorscheme files in ~/.config//nvim/colors/
colorscheme molokai

" NerdTree
let NERDTreeShowHidden=1
map <C-n> :NERDTreeToggle<CR>

" Enable/Disable Python Syntax Highlighting
let g:python_highlight_all = 1

" Recompile Suckless Programs Automatically
:au! BufWritePost *config.h ! make clean install %

" Set Leader Key
let mapleader=","
set timeout timeoutlen=1500

" Navigate around splits with a single key combo.
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" Location of viminfo
set viminfo+=n~/.conofig/nvim/viminfo
