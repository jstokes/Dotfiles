#!/bin/zsh

# if [[ "$TERM" == "dumb" ]]
# then
# 	unsetopt zle
# 	unsetopt prompt_cr
# 	unsetopt prompt_subst
# 	unfunction precmd
# 	unfunction preexec
# 	PS1='$ '
# 	return
# fi

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
export EDITOR='emacs'
export VISUAL='emacs'

DISABLE_AUTO_TITLE="true"
COMPLETION_WAITING_DOTS="true"
ZSH_THEME=""

source ~/.bin/tmuxinator.zsh

source ~/.zsh/git-prompt/zshrc.sh

PROMPT=$'%{${fg[green]}%}%B%~%b$(git_super_status)%{${fg[default]}%} '
ZSH_THEME_GIT_PROMPT_PREFIX=" ["
ZSH_THEME_GIT_PROMPT_SUFFIX="]"
ZSH_THEME_GIT_PROMPT_SEPARATOR="|"
ZSH_THEME_GIT_PROMPT_BRANCH="%{$fg_bold[blue]%}"
ZLE_REMOVE_SUFFIX_CHARS=$' \t\n;&'
GIT_PROMPT_EXECUTABLE="haskell"
ZSH_THEME_GIT_PROMPT_CACHE=true

source ~/.fzf.zsh

source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

export LEIN_SNAPSHOTS_IN_RELEASE=true

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

export ANDROID_HOME="$HOME/Library/Android/sdk"
