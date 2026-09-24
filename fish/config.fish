# =============================================================================
# Fish Shell Configuration
# =============================================================================

# -----------------------------------------------------------------------------
# 1. Environment & PATH (Interactive & Non-Interactive)
# -----------------------------------------------------------------------------

if test -d /opt/homebrew
    set -gx HOMEBREW_PREFIX /opt/homebrew
    set -gx HOMEBREW_CELLAR /opt/homebrew/Cellar
    set -gx HOMEBREW_REPOSITORY /opt/homebrew
    fish_add_path -g -m -p /opt/homebrew/bin /opt/homebrew/sbin
    if test -n "$MANPATH"
        set -gx MANPATH (string replace --regex '^:*(.*?):*$' ':$1' -- "$MANPATH")
    end
    if not set -q INFOPATH
        set INFOPATH ''
    end
    set -gx INFOPATH /opt/homebrew/share/info $INFOPATH
else if test -f /home/linuxbrew/.linuxbrew/bin/brew
    eval (/home/linuxbrew/.linuxbrew/bin/brew shellenv)
else if test -f /usr/local/bin/brew
    eval (/usr/local/bin/brew shellenv)
end

# System and tool PATH entries (fish_add_path deduplicates automatically)
fish_add_path /usr/local/sbin /usr/local/bin
fish_add_path "$HOME/.cargo/bin"
fish_add_path "$HOME/.local/bin"
fish_add_path "$HOME/.babashka/bbin/bin"
fish_add_path "$HOME/Developer/bin"
fish_add_path "/usr/local/git/bin"
fish_add_path "/usr/local/share/npm/bin"
fish_add_path "$HOME/.npm-global/bin"
fish_add_path "/usr/local/opt/ruby/bin"

# Ruby gems bin directory (fast check without booting Ruby VM)
for gemdir in /opt/homebrew/lib/ruby/gems/*/bin ~/.gem/ruby/*/bin
    if test -d "$gemdir"
        fish_add_path "$gemdir"
    end
end
if test -d "$HOME/Library/Python/3.12/bin"
    fish_add_path "$HOME/Library/Python/3.12/bin"
end
if test -d "/snap/bin"
    fish_add_path "/snap/bin"
end

# Bun
set -gx BUN_INSTALL "$HOME/.bun"
if test -d "$BUN_INSTALL/bin"
    fish_add_path "$BUN_INSTALL/bin"
end

# PNPM
if test -d "$HOME/Library/pnpm"
    set -gx PNPM_HOME "$HOME/Library/pnpm"
else if test -d "$HOME/.local/share/pnpm"
    set -gx PNPM_HOME "$HOME/.local/share/pnpm"
end
if set -q PNPM_HOME; and not contains "$PNPM_HOME" $PATH
    fish_add_path "$PNPM_HOME"
end

# ZVM
if test -d "$HOME/.zvm"
    set -gx ZVM_INSTALL "$HOME/.zvm/self"
    fish_add_path "$HOME/.zvm/bin"
    fish_add_path "$ZVM_INSTALL/"
end

# Local tool paths
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

# Environment variables
set -gx EDITOR nvim
set -gx VISUAL nvim
set -gx PAGER less
set -gx LESS '-F -g -i -M -R -S -w -X -z-4'

if string match -q "darwin*" $OSTYPE
    set -gx BROWSER open
end

if test -z "$LANG"
    set -gx LANG 'en_US.UTF-8'
end

set -gx GODEBUG asyncpreemptoff=1
set -gx TFENV_ARCH amd64

if test -d /opt/homebrew/opt/openjdk/libexec/openjdk.jdk/Contents/Home
    set -gx JAVA_HOME /opt/homebrew/opt/openjdk/libexec/openjdk.jdk/Contents/Home
else if test -x /usr/libexec/java_home
    set -gx JAVA_HOME (/usr/libexec/java_home -v 17 2>/dev/null)
end

if test -d /opt/homebrew/opt/maven/libexec
    set -gx M2_HOME /opt/homebrew/opt/maven/libexec
else if test -d /usr/local/opt/maven/libexec
    set -gx M2_HOME /usr/local/opt/maven/libexec
end

set -gx LEIN_SNAPSHOTS_IN_RELEASE "true"
set -gx JAVA_OPTS "$JAVA_OPTS -XX:-OmitStackTraceInFastThrow -XX:+UnlockDiagnosticVMOptions -XX:+DebugNonSafepoints"

# Named directories
set -g play "$HOME/play"
set -g work "$HOME/work"

# Temporary directory setup
if not test -d "$TMPDIR"
    set -gx TMPDIR "/tmp/$USER"
    mkdir -p -m 700 "$TMPDIR"
end

set -g TMPPREFIX (string trim -r -c / "$TMPDIR")"/fish"
if not test -d "$TMPPREFIX"
    mkdir -p "$TMPPREFIX"
end


# Local environment file if present
if test -f "$HOME/.local/bin/env.fish"
    source "$HOME/.local/bin/env.fish"
end

# Secrets config if present
if test -f ~/.config/fish/secrets.fish
    source ~/.config/fish/secrets.fish
else if test -f ~/.config/fish/conf.d/secrets.fish
    source ~/.config/fish/conf.d/secrets.fish
end

# Source work-specific config if it exists
if test -f ~/.config/fish/config.work.fish
    source ~/.config/fish/config.work.fish
else if test -f ~/.fishrc.work
    source ~/.fishrc.work
end


# -----------------------------------------------------------------------------
# 2. Interactive Shell Settings
# -----------------------------------------------------------------------------
if status is-interactive

    # GPG Agent
    if test -f ~/.gnupg/gpg-agent.conf; and type -q gpgconf
        gpgconf --launch gpg-agent
        set -gx GPG_TTY (tty)
    end

    # OrbStack shell integration
    if test -f ~/.orbstack/shell/init.fish
        source ~/.orbstack/shell/init.fish 2>/dev/null
    end

    # Enable vi key bindings
    fish_vi_key_bindings
    set -g fish_sequence_key_delay_ms 200

    ## Enable jj status
    set -g tide_left_prompt_items pwd vcs character
    set -g tide_right_prompt_items status cmd_duration context jobs

    # Load fzf shell integration
    if type -q fzf
        fzf --fish | source
    end

    # User key bindings (automatically executed by fish when key bindings initialize)
    function fish_user_key_bindings
        # Switch from insert mode to normal mode with 'fd'
        bind -M insert -m default fd backward-char force-repaint

        # Ensure fzf key bindings (including Ctrl+R for history search) apply in vi mode
        if functions -q fzf_key_bindings
            fzf_key_bindings
        end

        # Bind Ctrl+P to fzf file search (matching previous zsh fzf-file-widget binding)
        if functions -q fzf-file-widget
            bind \cp fzf-file-widget
            bind -M insert \cp fzf-file-widget
            bind -M default \cp fzf-file-widget
        else if functions -q __fzf_find_file
            bind \cp __fzf_find_file
            bind -M insert \cp __fzf_find_file
            bind -M default \cp __fzf_find_file
        end
    end

    # Apply bindings immediately
    fish_user_key_bindings

    # -------------------------------------------------------------------------
    # Aliases
    # -------------------------------------------------------------------------
    alias g='git'
    alias gdfm='git fetch; and git diff HEAD (git merge-base origin/master HEAD)'
    alias ec='emacsclient -c'
    alias ed='emacs --debug-init'
    alias ll='ls -lh'
    alias l='ls -1A'
    alias la='ll -A'
    alias lc='lt -c'
    alias lk='ll -Sr'
    alias lm='la | $PAGER'
    alias ln='ln -i'
    alias locate='locate'
    alias lr='ll -R'
    alias ls='ls -G'
    alias lt='ll -tr'
    alias lu='lt -u'
    alias lx='ll -XB'
    alias man='man'
    alias mkdir='mkdir -p'
    alias mv='mv -i'
    alias scp='scp'
    alias sftp='sftp'
    alias sl='ls'
    alias top='htop'
    alias type='type -a'
    alias dc='docker-compose'
    alias ldt='lein with-profile -user deps :tree 2>&1 | less -R'
    alias gh='PAGER=cat gh'

end


# -----------------------------------------------------------------------------
# 3. Helper Functions
# -----------------------------------------------------------------------------

function extract
    if test -f "$argv[1]"
        switch "$argv[1]"
            case '*.tar.bz2'
                tar xjf "$argv[1]"
            case '*.tar.gz'
                tar xzf "$argv[1]"
            case '*.bz2'
                bunzip2 "$argv[1]"
            case '*.rar'
                unrar e "$argv[1]"
            case '*.gz'
                gunzip "$argv[1]"
            case '*.tar'
                tar xf "$argv[1]"
            case '*.tbz2'
                tar xjf "$argv[1]"
            case '*.tgz'
                tar xzf "$argv[1]"
            case '*.zip'
                unzip "$argv[1]"
            case '*.Z'
                uncompress "$argv[1]"
            case '*.7z'
                7z x "$argv[1]"
            case '*'
                echo "'$argv[1]' cannot be extracted via extract()"
        end
    else
        echo "'$argv[1]' is not a valid file"
    end
end

function fname
    find . -iname "*$argv*"
end

function f
    rg -ir $argv ./
end

function ssh-copy-id
    cat ~/.ssh/id_rsa.pub | ssh "$argv[1]" "mkdir -p ~/.ssh/; cat >> ~/.ssh/authorized_keys"
end

function foreach
    while read -r l
        eval $argv
    end
end

function clr
    clear
end

# fzf helper functions
function vimf
    set -l FILE (fzf)
    if test -n "$FILE"
        vim "$FILE"
    end
end

function fda
    set -l target "."
    if test (count $argv) -gt 0
        set target $argv[1]
    end
    set -l DIR (find $target -type d 2> /dev/null | fzf)
    if test -n "$DIR"
        cd "$DIR"
    end
end

function fh
    set -l cmd (history | fzf +s | sed 's/ *[0-9]* *//')
    if test -n "$cmd"
        eval $cmd
    end
end

function fkill
    set -l sig "-9"
    if test (count $argv) -gt 0
        set sig "-$argv[1]"
    end
    set -l pids (ps -ef | sed 1d | fzf -m | awk '{print $2}')
    if test -n "$pids"
        echo $pids | xargs kill $sig
    end
end

# Workaround for Claude Code shopt issue
function shopt
    return 0
end

# agy shortcuts
function agyp
    agy --prompt-interactive $argv
end

function agydo
    agy --prompt $argv
end
