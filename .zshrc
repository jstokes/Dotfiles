#!/bin/zsh

for file in ~/env/.*
  do if [[ $file != *.swp* ]] then
    source "$file"
  fi
done 

ZSH_THEME_GIT_PROMPT_PREFIX="["         # Prefix at the very beginning of the prompt, before the branch name
ZSH_THEME_GIT_PROMPT_SUFFIX="]"             # At the very end of the prompt

bindkey -v
bindkey "^R" history-incremental-search-backward
bindkey "^S" history-incremental-search-forward
bindkey -M viins 'jk' vi-cmd-mode

autoload -U colors
autoload -U edit-command-line
zle -N edit-command-line
bindkey -M vicmd v edit-command-line
export EDITOR='mvim -v'
zstyle ':completion:*' hosts off

DISABLE_AUTO_TITLE="true"
COMPLETION_WAITING_DOTS="true"
ZSH=$HOME/.oh-my-zsh
ZSH_THEME="jeff"

plugins=(vi-mode gitfast bundle gradle)

source $ZSH/oh-my-zsh.sh

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

function git_prompt_info() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo "$ZSH_THEME_GIT_PROMPT_PREFIX${ref#refs/heads/}$(parse_git_dirty)$ZSH_THEME_GIT_PROMPT_SUFFIX"
}

#THIS MUST BE AT THE END OF THE FILE FOR GVM TO WORK!!!
[[ -s "/Users/jstokes/.gvm/bin/gvm-init.sh" && -z $(which gvm-init.sh | grep '/gvm-init.sh') ]] && source "/Users/jstokes/.gvm/bin/gvm-init.sh"
