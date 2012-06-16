#!/usr/bin/env ruby
# setup.rb

#list of dirs which you don't want to symlink
ignored = %w(.gitignore .gitmodules setup.rb README init installer)

current_dir = File.expand_path(Dir.pwd)
home_dir = File.expand_path("~")

links = `git ls-tree --name-only HEAD`.lines.map(&:strip).select {|x| !ignored.include?(x)  }

links.each do |link|
  link = File.join(current_dir, link)
  symlink = File.join(home_dir, File.basename(link))

  if File.exists?(symlink) 
    File.rename(symlink, "#{symlink}.bak")
  end

  `ln -ns #{link} #{symlink}`
end

