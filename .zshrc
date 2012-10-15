for file in ~/Dotfiles/env/.*
  do source $file
done 

bindkey -v
bindkey "^R" history-incremental-search-backward
bindkey -M viins 'jk' vi-cmd-mode

