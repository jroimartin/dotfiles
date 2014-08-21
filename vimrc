" Pathogen
execute pathogen#infect()

" Configuration
set wildmenu
set nocompatible
set nobackup
set ignorecase smartcase
set incsearch
set hlsearch
set ai
set cin
set showmatch
set nu
set backspace=2
set shiftwidth=4
set tabstop=4
set smarttab
"set expandtab
filetype on
filetype plugin indent on
syntax on
"colorscheme desert

" Run copen after using :grep or :make
autocmd QuickFixCmdPost * copen

" show commit diff
map <F12> :sp<cr>:e /tmp/commit.diff<cr>:r !git diff HEAD<cr>

" ctags support
map <F11> :!/usr/local/bin/ctags -f tags -R .

" Highlight chars over column 80
"match ErrorMsg '\%>80v.\+'

" status line
set laststatus=2 " always show the status bar
set statusline=%F\ %c.%l/%L\ %m

" Buffers
map gb :bn<CR>
map gB :bp<CR>

" Non-printable characters
set lcs=eol:$,tab:\|-
map \| :set invlist<CR>

" Search
map \ :nohlsearch<CR>

" Go
" Auto gofmt when saving
"autocmd FileType go autocmd BufWritePre <buffer> Fmt

" Ruby
autocmd FileType ruby,eruby set ts=2 sw=2 expandtab

" Encryption algorithm for vim -x
set cm=blowfish
