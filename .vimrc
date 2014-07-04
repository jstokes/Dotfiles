let g:dotvim_settings = {}
let g:dotvim_settings.version = 1
let vimpager_passthrough = 1
let vimpager_scrolloff = 5 

" here are some basic customizations, please refer to the top of the vimrc
" file for all possible options
let g:dotvim_settings.autocomplete_method = 'ycm'

" by default, language specific plugins are not loaded.  this can be changed with the following:
" let g:dotvim_settings.plugin_groups_exclude = ['ruby','python']

" alternatively, you can set this variable to load exactly what you want
let g:dotvim_settings.plugin_groups = [ 'core', 'ruby', 'scm', 'autocomplete', 'editing', 'navigation', 'unite', 'indents', 'textobj', 'misc' ]

" finally, load the distribution
source ~/.vim/vimrc

NeoBundle 'jszakmeister/vim-togglecursor'
NeoBundle 'Keithbsmiley/investigate.vim.git'
NeoBundle 'jceb/vim-orgmode'
NeoBundle 'tpope/vim-cucumber'
NeoBundle 'tpope/vim-classpath'
NeoBundle 'tpope/vim-fireplace'
NeoBundle 'ecomba/vim-ruby-refactoring'
NeoBundle 'ngmy/vim-rubocop'
NeoBundle 'tpope/vim-dispatch'
NeoBundle 'tpope/vim-sleuth'
NeoBundle 'Slava/vim-colors-tomorrow'
NeoBundle 'derekwyatt/vim-scala'

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
map ; :
let g:EclimCompletionMethod = 'omnifunc'
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1
let g:airline#extensions#eclim#enabled = 1
let g:ycm_key_list_select_completion=['<C-n>', '<Down>', '<Tab>']
let g:ycm_key_list_previous_completion=['<C-p>', '<Up>', '<S-Tab>']
" let g:syntastic_ruby_checkers = ['mri', 'rubocop', 'rubylint']
let g:syntastic_ruby_checkers = []
nnoremap <silent> <leader>gb :Gblame -w<CR>
set timeoutlen=500
" 
" Run currently open cucumber feature file
map <Leader>ct :w<cr>:!cucumber %<cr>
 
" Run current cucumber scenario
map <Leader>cl :w<cr>:exe "!cucumber %" . ":" . line(".")<cr>

" redraw to hopefully help artifacting
au BufWritePost * :silent! :syntax sync fromstart<cr>:redraw!<cr>

" Close buffer without killing split
nnoremap <leader>d :bp<bar>sp<bar>bn<bar>bd<CR> 
