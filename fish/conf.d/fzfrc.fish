function vimf
    set -l FILE (fzf)
    if test -n "$FILE"
        vim "$FILE"
    end
end

function fda
    set -l target "."
    if test (count $argv) -gt 0
        set target $argv[1]
    end
    set -l DIR (find $target -type d 2> /dev/null | fzf)
    if test -n "$DIR"
        cd "$DIR"
    end
end

function fh
    set -l cmd (history | fzf +s | sed 's/ *[0-9]* *//')
    if test -n "$cmd"
        eval $cmd
    end
end

function fkill
    set -l sig "-9"
    if test (count $argv) -gt 0
        set sig "-$argv[1]"
    end
    set -l pids (ps -ef | sed 1d | fzf -m | awk '{print $2}')
    if test -n "$pids"
        echo $pids | xargs kill $sig
    end
end
