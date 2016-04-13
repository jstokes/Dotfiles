export PATH=/Library/Frameworks/Python.framework/Versions/2.7/bin:/Library/Frameworks/Python.framework/Versions/2.7/bin:/usr/local/bin:/bin:/usr/sbin:/sbin:/usr/bin:~/Developer/maven/bin:~/Developer/mongo2.2/bin:/usr/X11/bin:/usr/local/git/bin:~/Developer/maven/bin:~/Developer/maven/bin:~/Developer/bin:$PATH


#THIS MUST BE AT THE END OF THE FILE FOR GVM TO WORK!!!
[[ -s "/Users/jstokes/.gvm/bin/gvm-init.sh" && -z $(which gvm-init.sh | grep '/gvm-init.sh') ]] && source "/Users/jstokes/.gvm/bin/gvm-init.sh"

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

# path to the DCOS CLI binary
if [[ "$PATH" != *"/Users/jstokes/work/rtdp-rtbconversions/dcos/bin"* ]];
  then export PATH=$PATH:/Users/jstokes/work/rtdp-rtbconversions/dcos/bin;
fi

# path to the DCOS CLI binary
if [[ "$PATH" != *"/Users/jstokes/Developer/bin/dcos/bin"* ]];
  then export PATH=$PATH:/Users/jstokes/Developer/bin/dcos/bin;
fi
