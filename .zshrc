#!/bin/zsh

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

# Cursor shape for vi modes
function zle-keymap-select {
  if [[ ${KEYMAP} == vicmd ]] || [[ $1 = 'block' ]]; then
    echo -ne '\e[2 q'  # Block cursor for normal mode
  elif [[ ${KEYMAP} == main ]] || [[ ${KEYMAP} == viins ]] || [[ $1 = 'beam' ]]; then
    echo -ne '\e[6 q'  # Beam cursor for insert mode
  fi
}
zle -N zle-keymap-select

function zle-line-init {
  echo -ne '\e[6 q'  # Start with beam cursor
}
zle -N zle-line-init

if [[ -z "${CLAUDECODE}" ]]; then
  source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi

setopt interactivecomments

HISTFILE=$HOME/.zsh_history
HISTSIZE=10000000
SAVEHIST=10000000

setopt APPEND_HISTORY
setopt EXTENDED_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_SAVE_NO_DUPS
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_VERIFY
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY
setopt autocd


if [ -f ~/.gnupg/gpg-agent.conf ] && command -v gpgconf &>/dev/null; then
  gpgconf --launch gpg-agent
  export GPG_TTY=$(tty)
fi

# bun completions
[ -s "/Users/jeff/.bun/_bun" ] && source "/Users/jeff/.bun/_bun"

export PATH="$PATH:$HOME/.babashka/bbin/bin"
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"
export PATH="$PATH:/Users/jeff/.local/bin"
export PATH=~/.npm-global/bin:$PATH

export EDITOR=nvim

# pnpm
export PNPM_HOME="/Users/jeff/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac

# Workaround for Claude Code shopt issue
shopt() {
  return 0
}

export PATH="$PATH:/Users/jeff/.lmstudio/bin"
fpath+=("$(brew --prefix)/share/zsh/site-functions")

autoload -U promptinit; promptinit
prompt pure
