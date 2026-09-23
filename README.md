# dotfiles

Personal configuration files for macOS and Linux/WSL, managed with symlinks.

## Setup

```bash
# Install dependencies
./init

# Symlink dotfiles to ~/
ruby setup.rb
```

`setup.rb` symlinks all tracked files into your home directory (e.g. `fish` to `~/.config/fish` and `ghostty` to `~/.config/ghostty`).

## What's included

| File/Dir | Purpose |
|---|---|
| `fish/` | Fish shell config: vi bindings, Fisher plugins (Tide prompt), aliases, functions, PATH |
| `.tmux.conf` | Tmux: prefix C-a, vim nav, nord-ish theme, tpm plugins, fish default shell |
| `ghostty/config` | Ghostty terminal configuration |
| `.gitconfig` | Git aliases, worktree helpers (`git wta`), sensible defaults |
| `.gitignore_global` | Global gitignore for OS files, editors, build artifacts |
| `.vimrc` | Vim configuration |
| `.lein/profiles.clj` | Leiningen profiles for Clojure development |
| `.lsp/config.edn` | clojure-lsp settings |
| `env/` | Shell aliases, environment and helper scripts |

## Work-specific config

Work-specific settings are loaded from gitignored files if they exist:

- **`~/.config/fish/config.work.fish`** or **`~/.fishrc.work`** -- sourced at the end of `config.fish`
- **`~/.gitconfig.work`** -- included by `.gitconfig` (work email override)
- **`~/env/.amperityrc`** -- sourced if present

## Dependencies

Installed via `./init` or manually:

- [Fish Shell](https://fishshell.com) -- primary shell
- [Fisher](https://github.com/jorgebucaran/fisher) -- fish plugin manager
- [Tide](https://github.com/IlanCosman/tide) -- fish prompt theme
- [tpm](https://github.com/tmux-plugins/tpm) -- tmux plugin manager
- [fzf](https://github.com/junegunn/fzf) -- fuzzy finder
- [git-smart](https://github.com/geelen/git-smart) -- smart-pull, smart-merge, smart-log
