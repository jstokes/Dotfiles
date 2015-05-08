#!/bin/zsh
autoload -U colors
colors

if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

bindkey -v
bindkey -M viins 'jk' vi-cmd-mode
bindkey "^R" history-incremental-search-backward
bindkey "^S" history-incremental-search-forward
bindkey -M vicmd " " fzf-history-widget
bindkey -M vicmd "/" fzf-history-widget
bindkey -M vicmd "^P" fzf-file-widget
bindkey -M viins "^P" fzf-file-widget

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
ZSH_THEME_GIT_PROMPT_PREFIX=" ("
ZSH_THEME_GIT_PROMPT_SUFFIX=")"
ZSH_THEME_GIT_PROMPT_SEPARATOR=" "
ZSH_THEME_GIT_PROMPT_BRANCH="%{$fg_bold[green]%}"
ZLE_REMOVE_SUFFIX_CHARS=$' \t\n;&'
source ~/.fzf.zsh


#rbenv
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
export RBENV_ROOT=/usr/local/var/rbenv
rbenv global 2.2.1

# init docker
$(boot2docker shellinit 2> /dev/null)
