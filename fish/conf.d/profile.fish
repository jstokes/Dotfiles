if string match -q "darwin*" $OSTYPE
    set -gx BROWSER 'open'
end

set -gx EDITOR 'nvim'
set -gx VISUAL 'nvim'
set -gx PAGER 'less'
alias gh 'PAGER=cat gh'

if test -z "$LANG"
    set -gx LANG 'en_US.UTF-8'
end

fish_add_path /usr/local/sbin /usr/local/bin

set -gx LESS '-F -g -i -M -R -S -w -X -z-4'

if type -q lesspipe.sh
    set -gx LESSOPEN "| /usr/bin/env lesspipe.sh %s 2>&-"
else if type -q lesspipe
    set -gx LESSOPEN "| /usr/bin/env lesspipe %s 2>&-"
end

if not test -d "$TMPDIR"
    set -gx TMPDIR "/tmp/$USER"
    mkdir -p -m 700 "$TMPDIR"
end

set -g TMPPREFIX (string trim -r -c / "$TMPDIR")"/zsh"
if not test -d "$TMPPREFIX"
    mkdir -p "$TMPPREFIX"
end

fish_add_path "$HOME/.cargo/bin"

if test -f /opt/homebrew/bin/brew
    eval (/opt/homebrew/bin/brew shellenv)
end

fish_add_path "/Users/jeff/.local/bin"

# Added by OrbStack: command-line tools and integration
if test -f ~/.orbstack/shell/init.fish
    source ~/.orbstack/shell/init.fish 2>/dev/null
end
