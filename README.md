# dotfiles

Personal configuration files for macOS, managed with symlinks.

## Setup

```bash
# Install dependencies
./init

# Symlink dotfiles to ~/
ruby setup.rb
```

`setup.rb` symlinks all tracked files into your home directory.

## What's included

| File/Dir | Purpose |
|---|---|
| `.zshrc` | Zsh config: vi bindings, history, prompt, fzf, syntax highlighting |
| `.zprofile` | Login shell setup: paths, editors, brew |
| `.zshenv` | Sources `env/.*` files, sets JAVA_HOME |
| `.tmux.conf` | Tmux: prefix C-a, vim nav, nord-ish theme, tpm plugins |
| `.gitconfig` | Git aliases, worktree helpers (`git wta`), sensible defaults |
| `.gitignore_global` | Global gitignore for OS files, editors, build artifacts |
| `.vimrc` | Vim configuration |
| `.lein/profiles.clj` | Leiningen profiles for Clojure development |
| `.lsp/config.edn` | clojure-lsp settings |
| `env/.envrc` | PATH and environment variables |
| `env/.aliasrc` | Shell aliases |
| `env/.functionrc` | Utility functions (extract, fname, f) |
| `env/.fzfrc` | fzf helper functions |

## Work-specific config

Work-specific settings are loaded from gitignored files if they exist:

- **`~/.zshrc.work`** -- sourced at the end of `.zshrc` (AWS profiles, vault switching, etc.)
- **`~/.gitconfig.work`** -- included by `.gitconfig` (work email override)
- **`~/env/.amperityrc`** -- sourced by `.zshenv` via the `env/.*` glob

## Dependencies

Installed via `./init` or manually:

- [Prezto](https://github.com/sorin-ionescu/prezto) -- zsh framework
- [tpm](https://github.com/tmux-plugins/tpm) -- tmux plugin manager
- [zsh-git-prompt](https://github.com/olivierverdier/zsh-git-prompt) -- git status in prompt
- [fzf](https://github.com/junegunn/fzf) -- fuzzy finder
- [zsh-syntax-highlighting](https://github.com/zsh-users/zsh-syntax-highlighting)
- [git-smart](https://github.com/geelen/git-smart) -- smart-pull, smart-merge, smart-log
