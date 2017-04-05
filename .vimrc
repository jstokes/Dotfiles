let g:dotvim_settings = {}
let g:dotvim_settings.version = 1
let vimpager_passthrough = 1
let vimpager_scrolloff = 5

" here are some basic customizations, please refer to the top of the vimrc
" file for all possible options
" let g:dotvim_settings.autocomplete_method = 'ycm'

" by default, language specific plugins are not loaded.  this can be changed with the following:
" let g:dotvim_settings.plugin_groups_exclude = ['ruby','python']

" alternatively, you can set this variable to load exactly what you want
let g:dotvim_settings.plugin_groups = [ 'core', 'ruby', 'scm', 'autocomplete', 'editing', 'navigation', 'unite', 'indents']
let g:dotvim_settings.colorscheme = 'tomorrow-night-bright'

" finally, load the distribution
source ~/.vim/vimrc
"
call neobundle#begin()
NeoBundle 'Slava/vim-colors-tomorrow'
NeoBundle 'kien/rainbow_parentheses.vim'
NeoBundle 'tpope/vim-sexp'
NeoBundle 'tpope/vim-repeat'
NeoBundle 'tpope/vim-surround'
NeoBundle 'qpkorr/vim-bufkill'
call neobundle#end()

let g:togglecursor_default = "block"
let g:togglecursor_insert = "underline"
let g:togglecursor_leave = "underline"
let g:togglecursor_disable_tmux = 0

" anything defined here are simply overrides
au BufNewFile,BufRead [vV]agrantfile        set filetype=ruby
au BufNewFile,BufRead *.org                 set filetype=org
au BufNewFile,BufRead *.scala               set filetype=scala

set clipboard=unnamed
set nolist
set novisualbell  " No blinking
set noerrorbells  " No noise.
set vb t_vb=".
set wildmode=longest:full
" set wildignore+=*/out/**
" let g:EclimCompletionMethod = 'omnifunc'
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1
let g:airline#extensions#eclim#enabled = 1
let g:ycm_key_list_select_completion=['<C-n>', '<Down>', '<Tab>']
let g:ycm_key_list_previous_completion=['<C-p>', '<Up>', '<S-Tab>']
let g:syntastic_ruby_checkers = []
let g:rbpt_colorpairs = [
  \['darkred',   '#c5c8c6'],
  \['darkgreen', '#b5bd68'],
  \['gray',      '#8abeb7'],
  \['darkred',   '#81a2be'],
  \['darkcyan',  '#b294bb'],
  \['darkgreen', '#cc6666'],
  \['darkblue',  '#de935f'],
  \['darkcyan',  '#f0c674']]

let g:rbpt_max = 16
let g:rbpt_loadcmd_toggle = 0

nnoremap <silent> <leader>gb :Gblame -w<CR>
set timeoutlen=500

au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces

" Save when losing focus
au FocusLost * :silent! wall
" Resize splits when the window is resized
au VimResized * :wincmd =
" Make sure Vim returns to the same line when you reopen a file.
" Thanks, Amit
augroup line_return
    au!
    au BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \     execute 'normal! g`"zvzz' |
        \ endif
augroup END
" Keep search matches in the middle of the window.
nnoremap n nzzzv
nnoremap N Nzzzv
" Easier to type, and I never use the default behavior.
noremap H ^
noremap L $
vnoremap L g_

" Close buffer without killing split
nnoremap <leader>d :bp<bar>sp<bar>bn<bar>bd<CR>

map <Leader>r :w<CR>:VimuxRunLastCommand<CR>
map <Leader>t :w<CR>:RunTests<CR>
map <Leader>at :w<CR>:RunAllTests<CR>
if exists('$TMUX')
  autocmd FileType ruby map <buffer> <Leader>f :RunRubyFocusedTest<CR>
  autocmd FileType ruby map <buffer> <Leader>t :RunAllRubyTests<CR>
  autocmd FileType cucumber map <Leader>f :RunFocusedCuke<CR>
  autocmd FileType cucumber map <Leader>t :RunAllCukes<CR>
endif

" Protect large files from sourcing and other overhead.
" Files become read only
let g:LargeFile=10
if !exists("my_auto_commands_loaded")
  let my_auto_commands_loaded = 1
  " Large files are > 10M
  " Set options:
  " eventignore+=FileType (no syntax highlighting etc
  " assumes FileType always on)
  " noswapfile (save copy of file)
  " bufhidden=unload (save memory when other file is viewed)
  " buftype=nowritefile (is read-only)
  " undolevels=-1 (no undo possible)
  let g:LargeFile = 1024 * 1024 * 10
  augroup LargeFile
    autocmd BufReadPre * let f=expand("<afile>") | if getfsize(f) > g:LargeFile | set eventignore+=FileType | setlocal noswapfile bufhidden=unload buftype=nowrite undolevels=-1 | else | set eventignore-=FileType | endif
  augroup END
endif
