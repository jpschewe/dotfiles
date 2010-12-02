set expandtab
set background=dark
set ignorecase "Ignore case when searching
set smartcase

syntax on

" show matching brackets
set showmatch

" display mode INSERT/REPLACE/...
set showmode

" Required to be able to use keypad keys and map missed escape sequences
set esckeys

" get easier to use and more user friendly vim defaults
" CAUTION: This option breaks some vi compatibility. 
"          Switch it off if you prefer real vi compatibility
set nocompatible

" allow backspacing over everything in insert mode 
set backspace=indent,eol,start

filetype on
set textwidth=72
set fo=cotqr
set ruler

" Make sure we start editing commit messages at the top
" autocmd BufNewFile,BufRead *.git/COMMIT_EDITMSG exe "normal 1G"
" autocmd FileType gitcommit exe "normal 1G"

function! s:JumpToLastPosition()
  " When editing a file, always jump to the last known cursor position. 
  " Don't do it when the position is invalid or when inside an event handler 
  " (happens when dropping a file on gvim). 
    if line("'\"") > 0 && line("'\"") <= line("$") | 
      exe "normal g`\"" | 
    endif 
endfunction

if &filetype != 'gitcommit'
  " jump to last position
  call s:JumpToLastPosition()
endif
