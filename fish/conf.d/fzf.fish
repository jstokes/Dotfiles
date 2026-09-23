if type -q fzf
    if fzf --fish >/dev/null 2>&1
        fzf --fish | source
    else if test -f /usr/share/doc/fzf/examples/key-bindings.fish
        source /usr/share/doc/fzf/examples/key-bindings.fish
    end

    # Bind Ctrl+P to fzf file search (matching previous zsh fzf-file-widget binding)
    if functions -q __fzf_find_file
        bind \cp __fzf_find_file
        bind -M insert \cp __fzf_find_file
        bind -M default \cp __fzf_find_file
    end
end
