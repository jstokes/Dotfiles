for file in ~/env/.*; do
    source "$file"
done
export GODEBUG=asyncpreemptoff=1
export TFENV_ARCH=amd64
export JAVA_HOME=$(/usr/libexec/java_home -v 17)

# ZVM
export ZVM_INSTALL="$HOME/.zvm/self"
export PATH="$HOME/.zvm/bin:$PATH"
export PATH="$PATH:$ZVM_INSTALL/"
