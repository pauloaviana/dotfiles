" Set Leader key
let mapleader="`"

" urlview - maps to /u
:noremap <leader>u :w<Home>silent <End> !urlview<CR>

" Set spell check
map <leader>s :setlocal spell! spelllang=en_gb<CR>

" Splits open at the bottom and right
set splitbelow splitright

" Check file in shellcheck:
map <leader>p :!clear && shellcheck %<CR>

" Save file as sudo on files that require root permission
cnoremap w!! execute 'silent! write !sudo tee % >/dev/null' <bar> edit!

" Toggle paste
set pastetoggle=<F2>

" Automatic text replacement
map S :%s//gI<Left><Left><Left>

" Disable autocomments on new lines
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

" Ignore case sensitivity and do a smart search
set ignorecase
set smartcase

" Change Tabs for spaces
set expandtab

" Do not create a backup file
set nobackup

" Set line numbers
:set number
syntax on

" Set cursor line
set cursorline

" colorscheme onedark

execute pathogen#infect()
filetype plugin indent on

"pathogen & Live-Latex auto compile"

