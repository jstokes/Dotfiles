#!/bin/zsh

# autoload -U promptinit; promptinit
# prompt pure

autoload -U colors
colors

if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

bindkey -v
bindkey -M viins 'fd' vi-cmd-mode
bindkey "^R" history-incremental-search-backward
bindkey "^S" history-incremental-search-forward
bindkey -M vicmd " " fzf-history-widget
bindkey -M vicmd "/" fzf-history-widget
bindkey -M vicmd "^P" fzf-file-widget
bindkey -M viins "^P" fzf-file-widget

autoload -U edit-command-line
zle -N edit-command-line
bindkey -M vicmd v edit-command-line

DISABLE_AUTO_TITLE="true"
COMPLETION_WAITING_DOTS="true"

source ~/.bin/tmuxinator.zsh
source ~/.zsh/git-prompt/zshrc.sh
source ~/.fzf.zsh
source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

PROMPT=$'%{${fg[green]}%}%B%~%b$(git_super_status)%{${fg[default]}%} '
ZSH_THEME_GIT_PROMPT_PREFIX=" ["
ZSH_THEME_GIT_PROMPT_SUFFIX="]"
ZSH_THEME_GIT_PROMPT_SEPARATOR="|"
ZSH_THEME_GIT_PROMPT_BRANCH="%{$fg_bold[blue]%}"
ZLE_REMOVE_SUFFIX_CHARS=$' \t\n;&'
GIT_PROMPT_EXECUTABLE="haskell"
ZSH_THEME_GIT_PROMPT_CACHE=true

setopt interactivecomments

HISTFILE=$HOME/.zsh_history
HISTSIZE=10000000
SAVEHIST=10000000

setopt APPEND_HISTORY
setopt EXTENDED_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_DUPS # ignore duplication command history list
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_SAVE_NO_DUPS
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_VERIFY
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY # share command history data
setopt autocd
export PATH="/usr/local/opt/terraform@0.13/bin:$PATH"
export PATH="/Users/jeff/.emacs.d/bin:$PATH"
export PATH="$PATH:$HOME/.babashka/bbin/bin"


export AWS_PROFILE=dev

aws-profile () {
  export AWS_PROFILE="$1"
  aws sso login
  aws configure list
}

aws-sts () {
  aws sts get-caller-identity > /dev/null
  if [ $? -ne 0 ]; then
    aws sso login
  fi
  eval "$(aws2-wrap --export)"
  aws configure list
}
