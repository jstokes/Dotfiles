set -gx GODEBUG asyncpreemptoff=1
set -gx TFENV_ARCH amd64
set -gx JAVA_HOME (/usr/libexec/java_home -v 17)

# ZVM
set -gx ZVM_INSTALL "$HOME/.zvm/self"
fish_add_path "$HOME/.zvm/bin"
fish_add_path "$ZVM_INSTALL/"
