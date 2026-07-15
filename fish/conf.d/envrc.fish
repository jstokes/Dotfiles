fish_add_path "/sbin"
fish_add_path "/usr/sbin"
fish_add_path "/bin"
fish_add_path "/usr/bin"
fish_add_path "/usr/local/bin"
fish_add_path "$HOME/Developer/bin"
fish_add_path "/usr/local/git/bin"
fish_add_path "/usr/X11/bin"
fish_add_path "/usr/local/share/npm/bin"
fish_add_path "/usr/local/mysql/bin"
fish_add_path "/usr/local/share/npm/lib/node_modules"
fish_add_path "/usr/local/opt/ruby/bin"
if type -q gem
    fish_add_path (gem environment gemdir)/bin
end
fish_add_path "/Users/jeff/Library/Python/3.12/bin"

set -gx TERM "screen-256color"
set -gx LEIN_SNAPSHOTS_IN_RELEASE "true"
set -gx EDITOR "nvim"
set -gx VISUAL "less"
set -gx LESS "-F -g -i -M -r -+S -w -X -z-4"

if type -q brew
    set -gx M2_HOME "(brew --prefix maven)/libexec"
end

set -gx JAVA_OPTS "$JAVA_OPTS -XX:-OmitStackTraceInFastThrow -XX:+UnlockDiagnosticVMOptions -XX:+DebugNonSafepoints"

# In fish, HISTCONTROL, HISTIGNORE, and PROMPT_COMMAND are handled differently or natively. 
# They are safely ignored here.

# hash -d in zsh creates named directories (~play).
# In fish, we can just set global variables:
set -g play "$HOME/play"
set -g work "$HOME/work"
