# Enable vi mode
fish_vi_key_bindings

if test -f ~/.gnupg/gpg-agent.conf; and type -q gpgconf
    gpgconf --launch gpg-agent
    set -gx GPG_TTY (tty)
end

fish_add_path "$HOME/.babashka/bbin/bin"
set -gx BUN_INSTALL "$HOME/.bun"
fish_add_path "$BUN_INSTALL/bin"
fish_add_path "/Users/jeff/.local/bin"
fish_add_path ~/.npm-global/bin

set -gx EDITOR nvim

# pnpm
set -gx PNPM_HOME "/Users/jeff/Library/pnpm"
if not contains "$PNPM_HOME" $PATH
    fish_add_path "$PNPM_HOME"
end

fish_add_path "/Users/jeff/.lmstudio/bin"

# Workaround for Claude Code shopt issue
function shopt
    return 0
end

# agy --prompt-interactive shortcut
function agyp
    agy --prompt-interactive $argv
end

# agy --prompt shortcut
function agydo
    agy --prompt $argv
end

# NOTE: The Zsh config used "prompt pure". 
# To get the pure prompt in Fish, you can install fisher and the pure-fish/pure theme:
# 1. curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | source && fisher install jorgebucaran/fisher
# 2. fisher install pure-fish/pure
