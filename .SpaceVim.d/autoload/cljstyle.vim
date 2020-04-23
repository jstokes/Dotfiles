function! cljstyle#fix() abort
  let cwd = getcwd()
  let winsave = winsaveview()
  execute "cd" . expand('%:p:h')
  :%!cljstyle pipe
  execute "cd" . cwd
  call winrestview(winsave)
endfunction
