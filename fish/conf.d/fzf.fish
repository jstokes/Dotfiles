if type -q fzf
    if fzf --fish >/dev/null 2>&1
        fzf --fish | source
    else if test -f /usr/share/doc/fzf/examples/key-bindings.fish
        source /usr/share/doc/fzf/examples/key-bindings.fish
    end

    function fish_user_key_bindings
        # Ensure fzf key bindings (including Ctrl+R for history search) are active in vi mode
        if functions -q fzf_key_bindings
            fzf_key_bindings
        end

        # Bind Ctrl+P to fzf file search (matching previous zsh fzf-file-widget binding)
        if functions -q fzf-file-widget
            bind \cp fzf-file-widget
            bind -M insert \cp fzf-file-widget
            bind -M default \cp fzf-file-widget
        else if functions -q __fzf_find_file
            bind \cp __fzf_find_file
            bind -M insert \cp __fzf_find_file
            bind -M default \cp __fzf_find_file
        end
    end

    # Apply bindings immediately
    fish_user_key_bindings
end
