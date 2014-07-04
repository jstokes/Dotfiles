#!/bin/zsh
autoload -U colors
colors

bindkey "^R" history-incremental-search-backward
bindkey "^S" history-incremental-search-forward
bindkey -M viins 'jk' vi-cmd-mode
bindkey -M vicmd " " fzf-history-widget
bindkey -M vicmd "/" fzf-history-widget
bindkey -M vicmd "^P" fzf-file-widget
bindkey -M viins "^P" fzf-file-widget

autoload -U edit-command-line
zle -N edit-command-line
bindkey -M vicmd v edit-command-line
export EDITOR='mvim -v'
zstyle ':completion:*' hosts off

#rbenv
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
export RBENV_ROOT=/usr/local/var/rbenv

DISABLE_AUTO_TITLE="true"
COMPLETION_WAITING_DOTS="true"
ZSH=$HOME/.oh-my-zsh
ZSH_THEME=""

plugins=(heroku colorize vagrant brew gitfast git-flow vi-mode bundle gradle grails)

source $ZSH/oh-my-zsh.sh
source ~/.bin/tmuxinator.zsh

source ~/.zsh/git-prompt/zshrc.sh
PROMPT=$'%{${fg[green]}%}%B%~%b$(git_super_status)%{${fg[default]}%} '
ZLE_REMOVE_SUFFIX_CHARS=$' \t\n;&'
source ~/.fzf.zsh
