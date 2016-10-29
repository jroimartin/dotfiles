" Pathogen
execute pathogen#infect()

" Set Leader key
let mapleader=","

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
set tabstop=8
set shiftwidth=8
set softtabstop=8
set autoindent
syntax on
filetype plugin indent on
set background=dark
colorscheme desert

" Enable matchit
runtime macros/matchit.vim

" Menu color
highlight Pmenu ctermfg=white ctermbg=darkgrey
highlight PmenuSel ctermfg=white ctermbg=darkred
" Search color
highlight IncSearch ctermfg=black ctermbg=yellow
highlight Search ctermfg=white ctermbg=darkred

" Buffers
nmap gb :bn<CR>
nmap gB :bp<CR>

" Non-printable characters
set lcs=eol:$,tab:\|-
nmap \| :set invlist<CR>

" Search
nmap \ :nohlsearch<CR>

" Langs
let g:go_fmt_command = "goimports"
let g:go_template_autocreate = 0
autocmd FileType go nmap <leader>n <Plug>(go-rename)
autocmd FileType go nmap <leader>r <Plug>(go-referrers)

autocmd FileType ruby,eruby set ts=2 sw=2 sts=2 expandtab

autocmd FileType python set ts=4 sw=4 sts=4 expandtab

" Encryption algorithm for vim -x
set cm=blowfish
