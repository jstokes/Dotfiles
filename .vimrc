set rtp+=~/.vim/bundle/vundle
call vundle#rc()

Bundle 'git@github.com:wincent/Command-T.git'
Bundle 'gmarik/vundle'
Bundle 'cucumber.zip'
Bundle 'git.zip'
Bundle 'fugitive.vim'
Bundle 'vim-ruby/vim-ruby'
Bundle 'tpope/vim-surround'
Bundle 'ack.vim'
Bundle 'The-NERD-Commenter'
Bundle 'Lokaltog/powerline', {'rtp': 'powerline/bindings/vim/'}
Bundle 'chriskempson/vim-tomorrow-theme'
Bundle 'topfunky/PeepOpen-EditorSupport', {'rtp': 'vim-peepopen/'}
Bundle 'scrooloose/syntastic'
Bundle 'airblade/vim-gitgutter'
Bundle 'Valloric/YouCompleteMe'
Bundle 'derekwyatt/vim-scala.git'

" Settings for VimClojure
let g:EasyMotion_leader_key = '<Leader>'

syntax on
filetype plugin indent on

set nocompatible
set expandtab
set tabstop=2
set shiftwidth=2
set autoindent
set smartindent
set guioptions-=m  "remove menu bar
set guioptions-=T  "remove toolbar
set guioptions-=r  "remove right-hand scroll bar
set mouse=a
set gdefault
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
set guifont=Droid\ Sans\ Mono\ for\ Powerline:h13
set lispwords+=defpartial,defpage
set laststatus=2  " Always show status line.
set ruler  " Show ruler
set showcmd " Display an incomplete command in the lower right corner of the Vim window
set autoread " Read from file when it changes on the FS
set backspace=2 
set t_Co=256
set laststatus=2 " Always display the statusline in all windows
set noshowmode " Hide the default mode text (e.g. -- INSERT -- below the statusline)

au BufRead,BufNewFile {Gemfile,Rakefile,Capfile,*.rake,config.ru}     set ft=ruby
au BufRead,BufNewFile {*.gradle}                                      set ft=groovy
au BufRead,BufNewFile {COMMIT_EDITMSG}                                set ft=gitcommit
au FocusLost * :wa
augroup CommandTExtension
  autocmd!
  autocmd FocusGained * CommandTFlush
  autocmd BufWritePost * CommandTFlush
augroup END

let mapleader=","
let g:EclimCompletionMethod = 'omnifunc'

inoremap jk <Esc>
noremap <silent><Leader>/ :nohls<CR>
noremap <Leader>= gg=G<CR>
"Ruby debug variable, puts 'var = #{var}'
nnoremap <Leader>dv viwyoputs "pa = #{pa}"
vnoremap < <gv
vnoremap > >gv
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
nnoremap <Leader>u :GundoToggle<CR>
noremap <LocalLeader># "ayiw:Ack <C-r>a<CR>
vnoremap <LocalLeader># "ay:Ack <C-r>a<CR>
nnoremap <down> :bprev<CR>
nnoremap <up> :bnext<CR>
nnoremap <left> :tabnext<CR>
nnoremap <right> :tabprev<CR>
nnoremap j gj
nnoremap k gk
nnoremap ; :
" Copy the full path of the current file to clipboard
nmap ,cp :let @*=expand("%:p")<CR>
nmap ,5 :Gblame<CR>
nmap ,a ggVG
nnoremap / /\v
vnoremap / /\v
nnoremap ? ?\v
vnoremap ? ?\v
map ,r :w\|Silent !echo "runhaskell %" > test-commands<cr>
map ,, :w\|Silent !echo "make test" > test-commands<cr> 

command! -nargs=1 Silent
      \ | execute ':silent !'.<q-args>
      \ | execute ':redraw!'

set background=dark
colorscheme tomorrow-night-eighties

