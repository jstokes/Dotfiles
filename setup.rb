#!/usr/bin/env ruby
# setup.rb
require 'fileutils'

# list of files/dirs which you don't want to symlink
ignored = %w(.gitignore .gitmodules setup.rb README README.md init installer)
current_dir = File.expand_path(Dir.pwd)
links = `git ls-tree --name-only HEAD`.lines.map(&:strip).select { |x| !ignored.include?(x) }

def get_symlink(file_name)
  home_dir = File.expand_path("~")
  case file_name
  when "fish", "ghostty", "jj"
    File.join(home_dir, ".config", file_name)
  else
    File.join(home_dir, file_name)
  end
end

links.each do |link|
  symlink = get_symlink(link)
  src = File.join(current_dir, link)
  next unless File.exist?(src)

  puts "linking #{src} -> #{symlink}"
  FileUtils.mkdir_p(File.dirname(symlink))

  if File.symlink?(symlink)
    File.unlink(symlink)
  elsif File.directory?(symlink)
    FileUtils.rm_rf(symlink)
  elsif File.exist?(symlink)
    File.unlink(symlink)
  end

  File.symlink(src, symlink)
end
