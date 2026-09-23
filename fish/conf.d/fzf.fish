if type -q fzf
    if fzf --fish >/dev/null 2>&1
        fzf --fish | source
    else if test -f /usr/share/doc/fzf/examples/key-bindings.fish
        source /usr/share/doc/fzf/examples/key-bindings.fish
    end
end
