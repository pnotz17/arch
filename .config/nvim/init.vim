" Vim Settings
set t_Co=256				" enable 256 colors, true colors
syntax on				" enable syntax highlighting
set ruler               		" enable line and column number of the cursor on right side of statusline
set number relativenumber		" enable “Hybrid” line numbers
set showmatch           		" highlight matching parentheses / brackets [{()}]
set incsearch 				" highlight the first string matched in a search
set cursorline				" highlight the current line
set cursorcolumn			" highlight the current column

" Theme
" put colorscheme files in ~/.config//nvim/colors/
colorscheme molokai

" Load plugins 
call plug#begin()
Plug 'preservim/nerdtree'
Plug 'preservim/nerdcommenter'
Plug 'ap/vim-css-color'
Plug 'ryanoasis/vim-devicons'
Plug 'itchyny/lightline.vim'
call plug#end()

" Set Leader Key
let mapleader=","

" Navigate around splits with a single key combo.
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" NerdTree
map <C-n> :NERDTreeToggle<CR>

" Recompile Suckless Programs Automatically
:au! BufWritePost *config.h ! make clean install %

" Location of viminfo
set viminfo+=n~/.config/nvim/viminfo
