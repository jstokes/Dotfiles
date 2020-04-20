function! myspacevim#before() abort
  set vimcompatible
  " allow modelines
  set modeline
  " check the first 20 lines in a file
  set modelines=20
  set ttyfast
  set ruler
  set formatoptions-=cro
  set autochdir
  filetype plugin indent on

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

  " shared clipboard please
  set clipboard=unnamedplus

  " No highlight search result on ESC
  noremap <silent> <Esc> :noh <CR>

  " Trim trailing space on save
  autocmd BufWritePre * :%s/\s\+$//e

  " Map fd/df to escape for smashing
  inoremap fd <Esc>
  inoremap df <Esc>
  nnoremap fd <Esc>
  cnoremap dd <C-C>
  nnoremap fd <Esc>
  cnoremap dd <C-C>

  " clap clap
  nnoremap <Space>cf :Clap files <CR>
  nnoremap <Space>cp :Clap gfiles <CR>
  nnoremap <Space>cb :Clap buffers <CR>
  nnoremap <Space>ct :Clap proj_tags <CR>

  " Clojure Config
  autocmd!
  autocmd FileType Clojure nnoremap <buffer> == <Plug>(iced_format)
  autocmd FileType Clojure nnoremap <buffer> =G <Plug>(iced_format_all)
  autocmd FileType Clojure nnoremap <buffer> K <Plug>(iced_document_popup_open)
  autocmd FileType Clojure nnoremap <buffer> <C-]> <Plug>(iced_def_jump)
  autocmd FileType Clojure nnoremap <buffer> <C-c><C-c> <Plug>(iced_eval)<Plug>(sexp_outer_list)``
  autocmd FileType Clojure nnoremap <buffer> <C-c><C-e> <Plug>(iced_eval)<Plug>(sexp_inner_element)``

  " Clojure configuration
  " Enable default key mappings for vim-iced
  let g:iced_enable_default_key_mappings = v:true

  " cljstyle formatting
  setlocal equalprg=cljstyle\ pipe
endfunction


function! myspacevim#after() abort
  let g:sexp_mappings = {'sexp_indent': '', 'sexp_indent_top': ''}

  " ## vim-iced keybindings
  call SpaceVim#mapping#space#def('nmap', ['m',"'"], '<Plug>(iced_connect)', 'Iced Connect', 0)
  call SpaceVim#mapping#space#def('nmap', ['m','"'], '<Plug>(iced_jack_in)', 'Iced Jack In', 0)

  " ### <Space>k Slurp/Barf
  let g:_spacevim_mappings_space.k = {'name' : '+Paredit'}
  call SpaceVim#mapping#space#def('nmap', ['k','s'], '<Plug>(iced_slurp)', 'Slurp', 0)
  call SpaceVim#mapping#space#def('nmap', ['k','b'], '<Plug>(iced_barf)', 'Barf', 0)

  " ### <Space>me Eval commands
  let g:_spacevim_mappings_space.e = {'name' : '+Eval'}
  call SpaceVim#mapping#space#def('nmap', ['e','i'], '<Plug>(iced_eval)<Plug>(sexp_inner_element)``', 'Eval inner element', 0)
  call SpaceVim#mapping#space#def('nmap', ['e','e'], '<Plug>(iced_eval)<Plug>(sexp_outer_list)``', 'Eval outer element', 0)
  call SpaceVim#mapping#space#def('nmap', ['e','t'], '<Plug>(iced_eval_outer_top_list)', 'Eval top list', 0)
  call SpaceVim#mapping#space#def('nmap', ['e','a'], '<Plug>(iced_eval_at_mark)', 'Eval at mark', 0)
  call SpaceVim#mapping#space#def('nmap', ['e','n'], '<Plug>(iced_eval_ns)', 'Eval namespace', 0)
  call SpaceVim#mapping#space#def('vmap', ['e','e'], '<Plug>(iced_eval_visual)', 'Eval visual', 0)
  call SpaceVim#mapping#space#def('nmap', ['e','p'], '<Plug>(iced_print_last)', 'Print last', 0)
  call SpaceVim#mapping#space#def('nmap', ['e','b'], '<Plug>(iced_require)', 'Add require', 0)
  call SpaceVim#mapping#space#def('nmap', ['e','B'], '<Plug>(iced_require_all)', 'Add require all', 0)
  call SpaceVim#mapping#space#def('nmap', ['e','u'], '<Plug>(iced_undef)', 'Undefine', 0)
  call SpaceVim#mapping#space#def('nmap', ['e','U'], '<Plug>(iced_undef_all_in_ns)', 'Undefine all in ns', 0)
  call SpaceVim#mapping#space#def('nmap', ['e','q'], '<Plug>(iced_interrupt)', 'Interrupt', 0)
  call SpaceVim#mapping#space#def('nmap', ['e','Q'], '<Plug>(iced_interrupt_all)', 'Interrupt all', 0)
  call SpaceVim#mapping#space#def('nmap', ['e','M'], '<Plug>(iced_macroexpand_outer_list)', 'Macroexpand outer list', 0)
  call SpaceVim#mapping#space#def('nmap', ['e','m'], '<Plug>(iced_macroexpand_1_outer_list)', 'Macroexpand1 outer list', 0)

  " ### <Space>mt Test commands
  let g:_spacevim_mappings_space.t = {'name' : '+Test'}
  call SpaceVim#mapping#space#def('nmap', ['t','t'], '<Plug>(iced_test_under_cursor)', 'Run test', 0)
  call SpaceVim#mapping#space#def('nmap', ['t','l'], '<Plug>(iced_test_rerun_last)', 'Rerun last test', 0)
  call SpaceVim#mapping#space#def('nmap', ['t','s'], '<Plug>(iced_test_spec_check)', 'Test spec check', 0)
  call SpaceVim#mapping#space#def('nmap', ['t','o'], '<Plug>(iced_test_buffer_open)', 'Open test buffer', 0)
  call SpaceVim#mapping#space#def('nmap', ['t','n'], '<Plug>(iced_test_ns)', 'Test namespace', 0)
  call SpaceVim#mapping#space#def('nmap', ['t','p'], '<Plug>(iced_test_all)', 'Test all', 0)
  call SpaceVim#mapping#space#def('nmap', ['t','r'], '<Plug>(iced_test_redo)', 'Test redo', 0)

  " ### <Space>ms output commands
  let g:_spacevim_mappings_space.s = {'name' : '+Output'}
  call SpaceVim#mapping#space#def('nmap', ['s','s'], '<Plug>(iced_stdout_buffer_open)', 'Open stdout', 0)
  call SpaceVim#mapping#space#def('nmap', ['s','l'], '<Plug>(iced_stdout_buffer_clear)', 'Clear stdout', 0)
  call SpaceVim#mapping#space#def('nmap', ['s','q'], '<Plug>(iced_stdout_buffer_close)', 'Close stdout', 0)

  " ### <Space>mr refactor commands
  let g:_spacevim_mappings_space.r = {'name' : '+Refactor'}
  let g:_spacevim_mappings_space.r.c = {'name' : '+Clean'}
  let g:_spacevim_mappings_space.r.t = {'name' : '+Thread'}
  let g:_spacevim_mappings_space.r.e = {'name' : '+Extract'}
  let g:_spacevim_mappings_space.r.m = {'name' : '+Move'}
  call SpaceVim#mapping#space#def('nmap', ['r','c','n'], '<Plug>(iced_clean_ns)', 'Clean ns', 0)
  call SpaceVim#mapping#space#def('nmap', ['r','c','a'], '<Plug>(iced_clean_all)', 'Clean all', 0)
  call SpaceVim#mapping#space#def('nmap', ['r','a','a'], '<Plug>(iced_add_arity)', 'Add arity', 0)
  call SpaceVim#mapping#space#def('nmap', ['r','a','m'], '<Plug>(iced_add_missing)', 'Add missing', 0)
  call SpaceVim#mapping#space#def('nmap', ['r','a','n'], '<Plug>(iced_add_ns)', 'Add ns', 0)
  call SpaceVim#mapping#space#def('nmap', ['r','t','f'], '<Plug>(iced_thread_first)', 'Thread first', 0)
  call SpaceVim#mapping#space#def('nmap', ['r','t','l'], '<Plug>(iced_thread_last)', 'Thread last', 0)
  call SpaceVim#mapping#space#def('nmap', ['r','e','f'], '<Plug>(iced_extract_function)', 'Extract function', 0)
  call SpaceVim#mapping#space#def('nmap', ['r','m','l'], '<Plug>(iced_move_to_let)', 'Move to let', 0)

  " ### <Space>mh help commands
  let g:_spacevim_mappings_space.m.h = {'name' : '+Help'}
  call SpaceVim#mapping#space#def('nmap', ['h','b'], '<Plug>(iced_document_open)', 'Open document', 0)
  call SpaceVim#mapping#space#def('nmap', ['h','u'], '<Plug>(iced_use_case_open)', 'Use case open', 0)
  call SpaceVim#mapping#space#def('nmap', ['h','n'], '<Plug>(iced_next_use_case)', 'Next use case', 0)
  call SpaceVim#mapping#space#def('nmap', ['h','N'], '<Plug>(iced_prev_use_case)', 'Prev use case', 0)
  call SpaceVim#mapping#space#def('nmap', ['h','q'], '<Plug>(iced_document_close)', 'Document close', 0)
  call SpaceVim#mapping#space#def('nmap', ['h','s'], '<Plug>(iced_source_popup_show)', 'Source popup', 0)
  call SpaceVim#mapping#space#def('nmap', ['h','S'], '<Plug>(iced_source_show)', 'Show source', 0)
  call SpaceVim#mapping#space#def('nmap', ['h','c'], '<Plug>(iced_clojuredocs_open)', 'Clojuredocs', 0)
  call SpaceVim#mapping#space#def('nmap', ['h','h'], '<Plug>(iced_command_palette)', 'Command pallete', 0)

  " ### <Space>mb browse commands
  let g:_spacevim_mappings_space.m.b = {'name' : '+Browse'}
  call SpaceVim#mapping#space#def('nmap', ['m','b','n'], '<Plug>(iced_browse_related_namespace)', 'Browse ns', 0)
  call SpaceVim#mapping#space#def('nmap', ['m','b','s'], '<Plug>(iced_browse_spec)', 'Browse spec', 0)
  call SpaceVim#mapping#space#def('nmap', ['m','b','t'], '<Plug>(iced_browse_test_under_cursor)', 'Test under cursor', 0)
  call SpaceVim#mapping#space#def('nmap', ['m','b','r'], '<Plug>(iced_browse_references)', 'Browse references', 0)
  call SpaceVim#mapping#space#def('nmap', ['m','b','d'], '<Plug>(iced_browse_dependencies)', 'Browse dependencies', 0)

  " ### <Space>mj jump commands
  let g:_spacevim_mappings_space.m.j = {'name' : '+Jump'}
  call SpaceVim#mapping#space#def('nmap', ['m','j','n'], '<Plug>(iced_jump_to_next_sign)', 'Jump to next sign', 0)
  call SpaceVim#mapping#space#def('nmap', ['m','j','N'], '<Plug>(iced_jump_to_prev_sign)', 'Jump to prev sign', 0)
  call SpaceVim#mapping#space#def('nmap', ['m','j','l'], '<Plug>(iced_jump_to_let)', 'Jump to let', 0)

  " ### <Space>md tap commands
  let g:_spacevim_mappings_space.m.d = {'name' : '+Taps'}
  call SpaceVim#mapping#space#def('nmap', ['m','d','b'], '<Plug>(iced_browse_tapped)', 'Browse tapped', 0)
  call SpaceVim#mapping#space#def('nmap', ['m','d','l'], '<Plug>(iced_clear_tapped)', 'Clear tapped', 0)
  call SpaceVim#mapping#space#def('nmap', ['m','*'], '<Plug>(iced_grep)', 'Iced grep', 0)
  call SpaceVim#mapping#space#def('nmap', ['m','/'], ':<C-u>IcedGrep<Space>', 'Iced grep', 0)
endfunction
