" [vim-plug] Load plugins
call plug#begin()
Plug 'preservim/nerdtree'
Plug 'preservim/nerdcommenter'
Plug 'ap/vim-css-color'
Plug 'dustractor/vimtkcolor'
Plug 'ryanoasis/vim-devicons'
Plug 'itchyny/lightline.vim'

call plug#end()

" Vim Appearance
" put colorscheme files in ~/.vim/colors/
colorscheme tigrana-256-dark      " good colorschemes:murphy,cabin,monochrome,darth,spacegray,evening,tigrana-256-dark,

" Personal settings
set number              		" show line numbers
set wrap                		" wrap lines
set encoding=utf-8      		" set encoding to UTF-8 (default was "latin1")
set mouse=a             		" enable mouse support (might not work well on Mac OS X)
set wildmenu            		" visual autocomplete for command menu
set lazyredraw          		" redraw screen only when we need to
set showmatch           		" highlight matching parentheses / brackets [{()}]
set laststatus=2        		" always show statusline (even with only single window)
set ruler               		" show line and column number of the cursor on right side of statusline
set novisualbell        		" blink cursor on error, instead of beeping
set cursorcolumn	    		" show location vertical
set cursorline		    		" show location horizontal
set number relativenumber               " Show absolute line numbers (cf. relativenumber)

" Key Bindings
let mapleader=","

" Navigate around splits with a single key combo.
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" NerdTree
map <C-n> :NERDTreeToggle<CR>

" Recompile Suckless Programs Automatically
autocmd BufWritePost *config.h !sudo make clean install %

set viminfo+=n~/.vim/viminfo
