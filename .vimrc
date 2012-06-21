set nocompatible
filetype off
syntax on               " enable syntax

set mouse=a

set incsearch hlsearch
set ignorecase  " Do case in sensitive matching with
set smartcase		" be sensitive when there's a capital letter
set backspace=indent,eol,start	" more powerful backspacing
set nowrap
set relativenumber

set wildmode=longest,list,full
set wildmenu

set showmatch  " Show matching brackets.
set matchtime=5  " Bracket blinking.
set novisualbell  " No blinking
set noerrorbells  " No noise.
set vb t_vb=".
set clipboard=unnamed

au BufRead,BufNewFile {Gemfile,Rakefile,Capfile,*.rake,config.ru}     set ft=ruby
au BufRead,BufNewFile {COMMIT_EDITMSG}                                set ft=gitcommit

" Source the vimrc file after saving it
if has("autocmd")
  autocmd bufwritepost .vimrc source $MYVIMRC
endif

set laststatus=2  " Always show status line.
set ruler  " Show ruler
set showcmd " Display an incomplete command in the lower right corner of the Vim window

set rtp+=~/.vim/bundle/vundle
call vundle#rc()

filetype plugin indent on

let mapleader=","
let g:SuperTabDefaultCompletionType = "context"

set guifont=Droid_Sans_Mono:h11

inoremap jk <Esc>
noremap <silent><Leader>/ :nohls<CR>

set expandtab
set tabstop=2
set shiftwidth=2
set autoindent
set smartindent

set guioptions-=m  "remove menu bar
set guioptions-=T  "remove toolbar
set guioptions-=r  "remove right-hand scroll bar

Bundle 'gmarik/vundle'
Bundle 'git://git.wincent.com/command-t.git'
Bundle "cucumber.zip"
Bundle "git.zip"
Bundle "fugitive.vim"
Bundle "vim-ruby/vim-ruby"
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-endwise'
Bundle 'ervandew/supertab'

" Ack
Bundle "ack.vim"
noremap <LocalLeader># "ayiw:Ack <C-r>a<CR>
vnoremap <LocalLeader># "ay:Ack <C-r>a<CR>

colorscheme tomorrow_night

