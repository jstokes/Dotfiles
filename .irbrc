if RUBY_ENGINE == 'ruby' 
  require 'rubygems'
  require 'interactive_editor'
  require 'wirble'
  require 'irb/completion'
  require 'pp'

  IRB.conf[:AUTO_INDENT] = true

  Wirble.init
  Wirble.colorize

  def clr
    system('clear')
  end

  alias :q :exit
end
