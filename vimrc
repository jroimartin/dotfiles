" Settings
set nocompatible
set nobackup
set ignorecase smartcase
set incsearch
set hlsearch
set showmatch
set wildmenu
set ruler
set nonumber
set noexpandtab
set backspace=2
set tabstop=8
set shiftwidth=8
set softtabstop=8
set synmaxcol=256
set autoindent

let mapleader = ","

filetype plugin indent on
syntax on

" Color scheme
colorscheme desert
set background=dark
" Menu color
highlight Pmenu ctermfg=white ctermbg=darkgrey
highlight PmenuSel ctermfg=white ctermbg=darkred
" Search color
highlight IncSearch ctermfg=black ctermbg=yellow
highlight Search ctermfg=white ctermbg=darkred
" Diff colors
highlight DiffAdd ctermfg=white ctermbg=darkgreen
highlight DiffDelete ctermfg=white ctermbg=darkred
highlight DiffChange ctermfg=white ctermbg=darkyellow
highlight DiffText ctermfg=white ctermbg=darkmagenta
" Extra whitespaces
highlight ExtraWhitespace ctermbg=darkred
match ExtraWhitespace /\s\+$/

" Extra packages
packadd matchit

" ag
if executable('ag')
	set grepprg=ag\ --nogroup\ --nocolor\ --column
	set grepformat=%f:%l:%c:%m
endif

" Non-printable characters
set lcs=eol:$,tab:\|-
nmap \| :set invlist<CR>

" Search
nmap \ :nohlsearch<CR>

" Splits
nmap <C-j> 2<C-w>+
nmap <C-k> 2<C-w>-

" Grep and open results in new tab
command -nargs=+ Grep grep <args> | tabe | copen

" Langs
autocmd FileType ruby,eruby set ts=2 sw=2 sts=2 expandtab
autocmd FileType python set ts=4 sw=4 sts=4 expandtab
autocmd FileType html,css,javascript set ts=2 sw=2 sts=2 expandtab
autocmd FileType yaml set ts=2 sw=2 sts=2 expandtab
autocmd FileType markdown set ts=2 sw=2 sts=2 expandtab
autocmd BufRead,BufNewFile *.hbs set ft=html ts=2 sw=2 sts=2 expandtab

" Lang exceptions
autocmd BufRead,BufNewFile */metasm/* set ts=8 sw=8 sts=8 noexpandtab
