for file in ~/env/.*; do
    source "$file"
done
export GODEBUG=asyncpreemptoff=1
export TFENV_ARCH=amd64
export JAVA_HOME=$(/usr/libexec/java_home -v 17)
