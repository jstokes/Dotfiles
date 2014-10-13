#!/usr/bin/env ruby
# setup.rb

#list of dirs which you don't want to symlink
ignored = %w(.gitignore .gitmodules setup.rb README init installer)

current_dir = File.expand_path(Dir.pwd)
links = `git ls-tree --name-only HEAD`.lines.map(&:strip).select {|x| !ignored.include?(x)  }

def different_locations
  {'personal' => '/Users/jstokes/.emacs.d/personal',
   'prelude-modules.el' => '/Users/jstokes/.emacs.d/prelude-modules.el'}
end

def get_symlink(file_name)
  if different_locations.include? file_name
    different_locations[file_name]
  else
    home_dir = File.expand_path("~")
    File.join(home_dir, file_name)
  end
end

links.each do |link|
  symlink = get_symlink(link)
  link = File.join(current_dir, link)

  if File.exists?(symlink)
    File.rename(symlink, "#{symlink}.bak")
  end

  puts "ln -ns #{link} #{symlink}"
  `ln -ns #{link} #{symlink}`
end
