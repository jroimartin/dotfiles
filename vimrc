" Pathogen
execute pathogen#infect()

" Configuration
set nocompatible
set nobackup
set ignorecase smartcase
set incsearch
set hlsearch
set showmatch
set wildmenu
set ruler
set nonumber
set backspace=2
set tabstop=4
set shiftwidth=4
set softtabstop=4
set autoindent
syntax on
filetype plugin indent on
set background=dark
colorscheme desert

" Menu color
highlight Pmenu ctermfg=white ctermbg=darkgrey
highlight PmenuSel ctermfg=white ctermbg=darkred
" Search color
highlight IncSearch ctermfg=black ctermbg=yellow
highlight Search ctermfg=white ctermbg=darkred
" Trailing spaces
highlight TrailingWhitespace ctermfg=white ctermbg=red
match TrailingWhitespace /\s\+$/

" Run copen after using :grep or :make
autocmd QuickFixCmdPost * copen

" Buffers
map gb :bn<CR>
map gB :bp<CR>

" Non-printable characters
set lcs=eol:$,tab:\|-
map \| :set invlist<CR>

" Search
map \ :nohlsearch<CR>

" Langs
let g:go_fmt_command = "goimports"
autocmd FileType ruby,eruby set ts=2 sw=2 sts=2 expandtab
autocmd FileType python set ts=4 sw=4 sts=4 expandtab

" Encryption algorithm for vim -x
set cm=blowfish
