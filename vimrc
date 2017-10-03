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
set cryptmethod=blowfish2
syntax on
filetype plugin indent on

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

" Enable matchit
runtime macros/matchit.vim

" Set Leader key
let mapleader=","

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
autocmd FileType html,css,javascript set ts=2 sw=2 sts=2 expandtab
autocmd BufEnter,BufNew *.hbs set ft=html ts=2 sw=2 sts=2 expandtab

" ctrlp.vim
let g:ctrlp_custom_ignore = {
	\ 'dir':  '\v[\/](bower_components|node_modules|vendor)$',
	\ }

" ag
if executable('ag')
	set grepprg=ag\ --nogroup\ --nocolor\ --column
	set grepformat=%f:%l:%c:%m
	let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
	let g:ctrlp_use_caching = 0
endif
