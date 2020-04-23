" shared clipboard please
set clipboard=unnamedplus
filetype plugin indent on
syntax on
set vimcompatible
" allow modelines
set modeline
" check the first 20 lines in a file
set modelines=20
set ttyfast
set ruler
set formatoptions-=cro
set autochdir

" search defaults
set gdefault
set incsearch
set hlsearch

" allow autocompletion for commands and menus
set wildmode=longest,list,full
set wildmenu

" ignore VCS directories
set wildignore+=.git,.svn,target

" suffixes that get lower priority when doing tab completion for filenames
set suffixes=~,.bak,.swp,.o,.so,.ko,.class,.log
