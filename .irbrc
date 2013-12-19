require 'rubygems'
require 'interactive_editor'
require 'irb/completion'
require 'ap'

if RUBY_ENGINE == 'ruby' 
  require 'wirble'
  Wirble.init
  Wirble.colorize
else 
  require 'irb/ext/colorize'
end


IRB.conf[:AUTO_INDENT] = true

def clr
  system('clear')
end

alias :q :exit

