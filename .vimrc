let g:dotvim_settings = {}
let g:dotvim_settings.version = 1

" here are some basic customizations, please refer to the top of the vimrc
" file for all possible options
let g:dotvim_settings.autocomplete_method = 'ycm'

" by default, language specific plugins are not loaded.  this can be changed with the following:
" let g:dotvim_settings.plugin_groups_exclude = ['ruby','python']

" alternatively, you can set this variable to load exactly what you want
let g:dotvim_settings.plugin_groups = [ 'core', 'unite', 'misc', 'indents', 'scm', 'autocomplete', 'ruby', 'javascript', 'web', 'editing' ]


" finally, load the distribution
source ~/.vim/vimrc

NeoBundle 'jszakmeister/vim-togglecursor'
NeoBundle 'Keithbsmiley/investigate.vim.git'
NeoBundle 'jceb/vim-orgmode'

let g:togglecursor_default = "block"
let g:togglecursor_insert = "underline"
let g:togglecursor_leave = "underline"
let g:togglecursor_disable_tmux = 0

" anything defined here are simply overrides
au BufNewFile,BufRead [vV]agrantfile        set filetype=ruby
au BufNewFile,BufRead *.org                 set filetype=org
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
nnoremap <silent> <leader>gb :Gblame -w<CR>
