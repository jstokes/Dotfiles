#!/usr/bin/env ruby
# setup.rb

#list of dirs which you don't want to symlink
ignored = %w(.gitignore .gitmodules setup.rb README init installer)
current_dir = File.expand_path(Dir.pwd)
links = `git ls-tree --name-only HEAD`.lines.map(&:strip).select {|x| !ignored.include?(x)  }

def get_symlink(file_name)
  home_dir = File.expand_path("~")
  if file_name == "fish"
    File.join(home_dir, ".config", "fish")
  else
    File.join(home_dir, file_name)
  end
end

links.each do |link|
  symlink = get_symlink(link)
  link = File.join(current_dir, link)

  puts "linking #{link} -> #{symlink}"
  `ln -nsFv #{link} #{symlink}`
end
