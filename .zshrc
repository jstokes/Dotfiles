# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

ZSH_THEME="robbyrussell"

##SLI STUFF
source ~/.zsh/.slirc
source ~/.zsh/.mavenrc

alias zshconfig="subl ~/.zshrc"
alias ohmyzsh="subl ~/.oh-my-zsh"

autoload -U colors
colors

alias subl='subl -n'

# Uncomment following line if you want to disable autosetting terminal title.
DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git git-flow ruby rails)

source $ZSH/oh-my-zsh.sh

if [[ -s "$HOME/.rvm/scripts/rvm" ]] ; then source "$HOME/.rvm/scripts/rvm" ; fi

# Customize to your needs...
export PATH=/Library/Frameworks/Python.framework/Versions/2.7/bin:/Library/Frameworks/Python.framework/Versions/2.7/bin:/Users/jstokes/.rvm/gems/ruby-1.9.2-p290/bin:/Users/jstokes/.rvm/gems/ruby-1.9.2-p290@global/bin:/Users/jstokes/.rvm/rubies/ruby-1.9.2-p290/bin:/Users/jstokes/.rvm/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/Users/jstokes/Developer/maven/bin:/Users/jstokes/Developer/mongo2.0.2/bin:/usr/X11/bin:/usr/local/git/bin://Users/jstokes/Developer/maven/bin:/Users/jstokes/Developer/maven/bin:/Users/jstokes/Developer/bin
