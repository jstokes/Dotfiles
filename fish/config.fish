# Enable vi mode
fish_vi_key_bindings

if test -f ~/.gnupg/gpg-agent.conf; and type -q gpgconf
    gpgconf --launch gpg-agent
    set -gx GPG_TTY (tty)
end

fish_add_path "$HOME/.babashka/bbin/bin"
set -gx BUN_INSTALL "$HOME/.bun"
fish_add_path "$BUN_INSTALL/bin"
fish_add_path "$HOME/.local/bin"
fish_add_path "$HOME/.npm-global/bin"

set -gx EDITOR nvim

# pnpm
if test -d "$HOME/Library/pnpm"
    set -gx PNPM_HOME "$HOME/Library/pnpm"
else if test -d "$HOME/.local/share/pnpm"
    set -gx PNPM_HOME "$HOME/.local/share/pnpm"
end
if set -q PNPM_HOME; and not contains "$PNPM_HOME" $PATH
    fish_add_path "$PNPM_HOME"
end

if test -d "$HOME/.lmstudio/bin"
    fish_add_path "$HOME/.lmstudio/bin"
end

if test -d "$HOME/.kimi-code/bin"
    fish_add_path "$HOME/.kimi-code/bin"
end

if test -d "$HOME/.opencode/bin"
    fish_add_path "$HOME/.opencode/bin"
end

if test -d "$HOME/go-local/go/bin"
    fish_add_path "$HOME/go-local/go/bin"
end

if test -f "$HOME/.local/bin/env.fish"
    source "$HOME/.local/bin/env.fish"
end

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

# Source work-specific config if it exists
if test -f ~/.config/fish/config.work.fish
    source ~/.config/fish/config.work.fish
else if test -f ~/.fishrc.work
    source ~/.fishrc.work
end
