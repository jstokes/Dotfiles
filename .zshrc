#!/bin/zsh

for file in ~/env/.*
  do source "$file"
done 

ZSH_THEME_GIT_PROMPT_PREFIX="["         # Prefix at the very beginning of the prompt, before the branch name
ZSH_THEME_GIT_PROMPT_SUFFIX="]"             # At the very end of the prompt

bindkey -v
bindkey "^R" history-incremental-search-backward
bindkey "^S" history-incremental-search-forward
bindkey -M viins 'jk' vi-cmd-mode

autoload -U colors
autoload -U edit-command-line
zle -N edit-command-line
bindkey -M vicmd v edit-command-line
export EDITOR='mvim -v'
zstyle ':completion:*' hosts off

DISABLE_AUTO_TITLE="true"
COMPLETION_WAITING_DOTS="true"
ZSH=$HOME/.oh-my-zsh
ZSH_THEME="robbyrussell"


plugins=(vi-mode git bundle gradle)

source $ZSH/oh-my-zsh.sh
export JAVA_OPTS="-Xmx2g -XX:MaxPermSize=512m"
export PATH=/usr/local/Cellar/ruby/1.9.3-p362/bin:/usr/local/share/npm/bin:/Users/jstokes/Developer/bin/apache-activemq-5.7.0/bin:/usr/local/mysql/bin:/Library/Frameworks/Python.framework/Versions/2.7/bin:/Library/Frameworks/Python.framework/Versions/2.7/bin:/Users/jstokes/.rvm/gems/ruby-1.9.2-p290/bin:/Users/jstokes/.rvm/gems/ruby-1.9.2-p290@global/bin:/Users/jstokes/.rvm/rubies/ruby-1.9.2-p290/bin:/Users/jstokes/.rvm/bin:/usr/local/bin:/bin:/usr/sbin:/sbin:/usr/bin:/Users/jstokes/Developer/maven/bin:/Users/jstokes/Developer/mongo2.2/bin:/usr/X11/bin:/usr/local/git/bin:/Users/jstokes/Developer/maven/bin:/Users/jstokes/Developer/maven/bin:/Users/jstokes/Developer/bin:/Users/jstokes/.rvm/bin:/Users/jstokes/.rvm/rubies/ruby-1.9.3-p194/bin:/Users/jstokes/.rvm/gems/ruby-1.9.3-p194/bin:/Users/jstokes/.rvm/gems/ruby-1.9.3-p194@global/bin:/Applications/apache-jmeter-2.8/bin
export PATH=$(cd $(which gem)/..; pwd):$PATH

#THIS MUST BE AT THE END OF THE FILE FOR GVM TO WORK!!!
[[ -s "/Users/jstokes/.gvm/bin/gvm-init.sh" && -z $(which gvm-init.sh | grep '/gvm-init.sh') ]] && source "/Users/jstokes/.gvm/bin/gvm-init.sh"
