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
set clipboard=unnamed
set mouse=a
set tabstop=8
set shiftwidth=8
set softtabstop=8
set autoindent
syntax on
filetype plugin indent on

" Solarized + iTerm2
if $ITERM_PROFILE == "dark"
	set background=dark
else
	set background=light
endif
colorscheme solarized

" Enable matchit
runtime macros/matchit.vim

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
