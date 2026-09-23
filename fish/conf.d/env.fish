set -gx GODEBUG asyncpreemptoff=1
set -gx TFENV_ARCH amd64
if test -x /usr/libexec/java_home
    set -gx JAVA_HOME (/usr/libexec/java_home -v 17)
end

# ZVM
set -gx ZVM_INSTALL "$HOME/.zvm/self"
fish_add_path "$HOME/.zvm/bin"
fish_add_path "$ZVM_INSTALL/"
