" Settings
set nocompatible
set nobackup
set hlsearch
set showmatch
set wildmenu
set ruler
set nonumber
set expandtab
set backspace=2
set tabstop=4
set shiftwidth=4
set softtabstop=4
set synmaxcol=256
set autoindent
set autoread
set nofoldenable
set nomodeline
set mouse=a

let mapleader=','

filetype plugin indent on
syntax on

" Colors
set background=dark
" Menu color
highlight Pmenu ctermfg=white ctermbg=darkgrey
highlight PmenuSel ctermfg=white ctermbg=darkred
" Search color
highlight Search ctermfg=white ctermbg=darkred
" Diff colors
highlight DiffAdd ctermfg=white ctermbg=28
highlight DiffDelete ctermfg=white ctermbg=88
highlight DiffChange ctermfg=white ctermbg=darkyellow
highlight DiffText ctermfg=white ctermbg=darkmagenta

" Extra whitespaces
highlight ExtraWhitespace ctermbg=darkred
match ExtraWhitespace /\s\+$/

" Highlight column 80
highlight ColorColumn ctermbg=234
set colorcolumn=80

" Extra packages
packadd matchit

" Non-printable characters
set lcs=eol:$,tab:\|-
nnoremap \| :set invlist<CR>

" Search
nnoremap \ :nohlsearch<CR>

" Splits
nnoremap <C-j> 2<C-w>+
nnoremap <C-k> 2<C-w>-

" X clipboard
noremap <Leader>y "+y

" ctags
nnoremap <Leader>] <C-w>g]

" Grep and open results in new tab
command -nargs=+ Grep tabe | silent lgrep <args> | lopen | redraw!

" Linux kernel development
inoremap <F12> Signed-off-by: Roi Martin <jroi.martin@gmail.com>

" ripgrep
if executable('rg')
	set grepprg=rg\ --vimgrep
endif

" Langs
autocmd FileType ruby,eruby setlocal ts=2 sw=2 sts=2 expandtab
autocmd FileType python setlocal ts=4 sw=4 sts=4 expandtab
autocmd FileType html,css,javascript setlocal ts=2 sw=2 sts=2 expandtab
autocmd FileType yaml setlocal ts=2 sw=2 sts=2 expandtab
autocmd FileType markdown setlocal ts=2 sw=2 sts=2 expandtab
autocmd FileType go setlocal ts=8 sw=8 sts=8 noexpandtab
autocmd FileType rust setlocal ts=4 sw=4 sts=4 expandtab textwidth=79
autocmd BufRead,BufNewFile *.hbs setlocal ft=html ts=2 sw=2 sts=2 expandtab

" Go: golang.org/x/tools/cmd/goimports
if executable('goimports')
	autocmd BufWritePost *.go !goimports -w %
endif
