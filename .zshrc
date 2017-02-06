#!/bin/zsh
autoload -U colors
colors

if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

bindkey -v
bindkey -M viins 'fd' vi-cmd-mode
bindkey "^R" history-incremental-search-backward
bindkey "^S" history-incremental-search-forward

autoload -U edit-command-line
zle -N edit-command-line
bindkey -M vicmd v edit-command-line
export EDITOR='vi'
export VISUAL='vi'
zstyle ':completion:*' hosts off

DISABLE_AUTO_TITLE="true"
COMPLETION_WAITING_DOTS="true"
ZSH_THEME=""

source ~/.bin/tmuxinator.zsh

for file in ~/env/.*
do
  source "$file"
done

source ~/.zsh/git-prompt/zshrc.sh
PROMPT=$'%{${fg[blue]}%}%B%~%b$(git_super_status)%{${fg[default]}%} '
bindkey -M vicmd " " fzf-history-widget
bindkey -M vicmd "/" fzf-history-widget
bindkey -M vicmd "^P" fzf-file-widget
bindkey -M viins "^P" fzf-file-widget
