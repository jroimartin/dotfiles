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
set tabstop=8
set shiftwidth=8
set autoindent
syntax off
filetype plugin indent on
set background=dark
colorscheme desert

" Menu color
highlight Pmenu ctermfg=white ctermbg=darkgrey
highlight PmenuSel ctermfg=white ctermbg=darkred
" Search color
highlight IncSearch ctermfg=white ctermbg=darkblue
highlight Search ctermfg=white ctermbg=darkred

" Run copen after using :grep or :make
autocmd QuickFixCmdPost * copen

" show commit diff
map <F12> :sp<cr>:e /tmp/commit.diff<cr>:r !git diff HEAD<cr>

" ctags support
map <F11> :!/usr/local/bin/ctags -f tags -R .

" Highlight column 81
"set textwidth=80 formatoptions=q wrapmargin=0
"set cc=+1
"hi ColorColumn ctermbg=grey ctermfg=black cterm=bold

" Buffers
map gb :bn<CR>
map gB :bp<CR>

" Non-printable characters
set lcs=eol:$,tab:\|-
map \| :set invlist<CR>

" Search
map \ :nohlsearch<CR>

" Go
let g:go_fmt_command = "goimports"

" Ruby
autocmd FileType ruby,eruby set ts=2 sw=2 expandtab

" Encryption algorithm for vim -x
set cm=blowfish
