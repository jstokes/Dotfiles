#!/bin/sh
# Cross-platform clipboard helper for tmux (macOS, WSL, Wayland, X11)

copy() {
  if command -v pbcopy >/dev/null 2>&1; then
    pbcopy
  elif command -v clip.exe >/dev/null 2>&1; then
    clip.exe
  elif [ -x /mnt/c/WINDOWS/system32/clip.exe ]; then
    /mnt/c/WINDOWS/system32/clip.exe
  elif command -v wl-copy >/dev/null 2>&1; then
    wl-copy
  elif command -v xclip >/dev/null 2>&1; then
    xclip -selection clipboard -in
  else
    cat
  fi
}

if [ "$1" = "--daemon" ]; then
  while true; do
    if test -n "`tmux showb 2> /dev/null`"; then
      tmux saveb - | copy && tmux deleteb
    fi
    sleep 0.5
  done
else
  copy
fi
