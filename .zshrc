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

DISABLE_AUTO_TITLE="true"
COMPLETION_WAITING_DOTS="true"

if [[ -z "${CLAUDECODE}" ]]; then
  source ~/.bin/tmuxinator.zsh
  source ~/.zsh/git-prompt/zshrc.sh
  source ~/.fzf.zsh
  # Try Homebrew paths for zsh-syntax-highlighting (Apple Silicon then Intel)
  if [[ -f /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]]; then
    source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
  elif [[ -f /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]]; then
    source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
  fi

  PROMPT=$'%{${fg[green]}%}%B%~%b$(git_super_status)%{${fg[default]}%} '
  ZSH_THEME_GIT_PROMPT_PREFIX=" ["
  ZSH_THEME_GIT_PROMPT_SUFFIX="]"
  ZSH_THEME_GIT_PROMPT_SEPARATOR="|"
  ZSH_THEME_GIT_PROMPT_BRANCH="%{$fg_bold[blue]%}"
  ZLE_REMOVE_SUFFIX_CHARS=$' \t\n;&'
  GIT_PROMPT_EXECUTABLE="haskell"
  ZSH_THEME_GIT_PROMPT_CACHE=true
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

export PATH="$PATH:$HOME/.babashka/bbin/bin"

aws-profile () {
  if [[ -z $1 ]]; then
    echo "Usage: aws-profile <profile>"
    return 1
  fi
  export AWS_PROFILE="$1"
  region=$(aws configure get region --profile "$AWS_PROFILE")
  [[ -n $region ]] && export AWS_REGION="$region" AWS_DEFAULT_REGION="$region"
  aws sts get-caller-identity --output text >/dev/null 2>&1
  sts_rc=$?
  case $sts_rc in
    0)
      echo "Credentials / SSO token still valid for profile $AWS_PROFILE"
      ;;
    255)
      echo "Cached SSO token is expired – running aws sso login for $AWS_PROFILE …"
      if ! aws sso login --profile "$AWS_PROFILE"; then
        echo "ERROR: aws sso login failed for profile $AWS_PROFILE"
        return 1
      fi
      ;;
    *)
      echo "aws sts get-caller-identity failed (exit $sts_rc)."
      return $sts_rc
      ;;
  esac
  echo "Using profile $AWS_PROFILE"
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

# Shell-GPT integration ZSH v0.2
_sgpt_zsh() {
if [[ -n "$BUFFER" ]]; then
    _sgpt_prev_cmd=$BUFFER
    BUFFER+="⌛"
    zle -I && zle redisplay
    BUFFER=$(sgpt --shell <<< "$_sgpt_prev_cmd" --no-interaction)
    zle end-of-line
fi
}
zle -N _sgpt_zsh
bindkey ^k _sgpt_zsh

if [ -f ~/.gnupg/gpg-agent.conf ] && command -v gpgconf &>/dev/null; then
  gpgconf --launch gpg-agent
  export GPG_TTY=$(tty)
fi

# bun completions
[ -s "/Users/jeff/.bun/_bun" ] && source "/Users/jeff/.bun/_bun"

export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"

export PATH="$PATH:/Users/jeff/.local/bin"
export PATH=~/.npm-global/bin:$PATH

alias cc='claude'

export EDITOR=nvim

# opencode
export PATH=/Users/jeff/.opencode/bin:$PATH

# Added by Antigravity
export PATH="/Users/jeff/.antigravity/antigravity/bin:$PATH"

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

[[ -f ~/.safe-chain/scripts/init-posix.sh ]] && source ~/.safe-chain/scripts/init-posix.sh

alias claude-mem='bun "/Users/jeff/.claude/plugins/cache/thedotmack/claude-mem/10.5.6/scripts/worker-service.cjs"'

# Source work-specific config if it exists
[[ -f ~/.zshrc.work ]] && source ~/.zshrc.work
