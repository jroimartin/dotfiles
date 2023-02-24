set nocompatible
set noincsearch
set hlsearch
set showmatch
set ruler
set backspace=2
set nowrap
set synmaxcol=256
set autoindent
set nofoldenable
set nomodeline
set mouse=a
set ttymouse=xterm2
set wildmode=full
set wildmenu

filetype on
filetype plugin off
filetype indent off

let mapleader=','

" Colors
set background=dark
syntax on
highlight Pmenu ctermfg=white ctermbg=darkgrey
highlight PmenuSel ctermfg=white ctermbg=darkred
highlight Search ctermfg=white ctermbg=darkred
highlight DiffAdd ctermfg=white ctermbg=28
highlight DiffDelete ctermfg=white ctermbg=88
highlight DiffChange ctermfg=white ctermbg=darkyellow
highlight DiffText ctermfg=white ctermbg=darkmagenta

" 80 columns
set colorcolumn=80
highlight ColorColumn ctermbg=234

" Non-printable characters
set listchars=eol:$,tab:\|-,trail:-,extends:>,precedes:<
nnoremap \| :set invlist<CR>

" Search
nnoremap \ :nohlsearch<CR>

" Splits
nnoremap <C-j> 2<C-w>+
nnoremap <C-k> 2<C-w>-

" X clipboard
noremap <Leader>y "+y

" Grep
command -nargs=+ Grep tabe | silent lgrep <args> | lopen | redraw!
nnoremap <Leader>g :execute ':Grep' '-w' expand('<cword>')<CR>

if executable('rg')
	set grepprg=rg\ --vimgrep
endif

" netrw messages in one line window
let g:netrw_use_errorwindow=1

" Langs
set noexpandtab
set tabstop=8
set shiftwidth=8
set softtabstop=8

augroup VimrcLangs
	autocmd!
	autocmd FileType ruby,eruby setlocal ts=2 sw=2 sts=2 expandtab
	autocmd FileType python setlocal ts=4 sw=4 sts=4 expandtab
	autocmd FileType html,css,javascript setlocal ts=2 sw=2 sts=2 expandtab
	autocmd FileType json setlocal ts=2 sw=2 sts=2 expandtab
	autocmd FileType yaml setlocal ts=2 sw=2 sts=2 expandtab
	autocmd FileType markdown setlocal ts=2 sw=2 sts=2 expandtab
	autocmd FileType go setlocal ts=8 sw=8 sts=8 noexpandtab
	autocmd FileType rust setlocal ts=4 sw=4 sts=4 expandtab
		\ comments=s0:/*!,m:\ ,ex:*/,s1:/*,mb:*,ex:*/,:///,://!,://
	autocmd BufRead,BufNewFile *.toml setlocal ts=4 sw=4 sts=4 expandtab
	autocmd BufRead,BufNewFile *.hbs setlocal ft=html ts=2 sw=2 sts=2 expandtab
augroup END
