if [[ -f /opt/homebrew/bin/brew ]]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
elif [[ -f /home/linuxbrew/.linuxbrew/bin/brew ]]; then
  eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
fi

[ -f "$HOME/.local/bin/env" ] && . "$HOME/.local/bin/env"

# Added by LM Studio CLI (lms)
export PATH="$PATH:$HOME/.lmstudio/bin"
# End of LM Studio CLI section

# Added by Antigravity CLI installer
export PATH="$HOME/.local/bin:$PATH"

if [[ $- == *i* ]] && command -v fish >/dev/null 2>&1; then
  exec fish
fi
