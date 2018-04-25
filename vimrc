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

filetype on
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

" Non-printable characters
set lcs=eol:$,tab:\|-
nmap \| :set invlist<CR>

" Search
nmap \ :nohlsearch<CR>

" Langs
autocmd FileType ruby,eruby set ts=2 sw=2 sts=2 expandtab
autocmd FileType python set ts=4 sw=4 sts=4 expandtab
autocmd FileType html,css,javascript set ts=2 sw=2 sts=2 expandtab
autocmd FileType yaml set ts=2 sw=2 sts=2 expandtab
autocmd FileType markdown set ts=2 sw=2 sts=2 expandtab
autocmd BufEnter,BufNew *.hbs set ft=html ts=2 sw=2 sts=2 expandtab

" ag
if executable('ag')
	set grepprg=ag\ --nogroup\ --nocolor\ --column
	set grepformat=%f:%l:%c:%m
endif
