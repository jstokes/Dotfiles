# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh
ZSH_THEME="sorin"

##SLI STUFF
source ~/.zsh/.slirc
source ~/.zsh/.mavenrc

alias zshconfig="subl ~/.zshrc"
alias ohmyzsh="subl ~/.oh-my-zsh"

hash -d tomcat=/Library/Tomcat/

autoload -U colors

alias subl='subl -n'

# Uncomment following line if you want to disable autosetting terminal title.
DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git git-flow ruby rails bundler)

source $ZSH/oh-my-zsh.sh

if [[ -s "$HOME/.rvm/scripts/rvm" ]] ; then source "$HOME/.rvm/scripts/rvm" ; fi

# Customize to your needs...
export PATH=/Library/Frameworks/Python.framework/Versions/2.7/bin:/Library/Frameworks/Python.framework/Versions/2.7/bin:~/.rvm/gems/ruby-1.9.2-p290/bin:~/.rvm/gems/ruby-1.9.2-p290@global/bin:~/.rvm/rubies/ruby-1.9.2-p290/bin:~/.rvm/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:~/Developer/maven/bin:~/Developer/mongo2.0.2/bin:/usr/X11/bin:/usr/local/git/bin:~/Developer/maven/bin:~/Developer/maven/bin:~/Developer/bin
