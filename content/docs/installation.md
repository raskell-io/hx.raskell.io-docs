+++
title = "Installation"
weight = 1
+++

## Quick Install

The fastest way to install hx is via the install script:

```bash
curl -fsSL https://get.raskell.io/hx | sh
```

## Manual Installation

### From GitHub Releases

Download the latest release for your platform from [GitHub Releases](https://github.com/raskell-io/hx/releases).

### From Source

```bash
git clone https://github.com/raskell-io/hx
cd hx
cargo build --release
```

## Verify Installation

```bash
hx --version
```

## Shell Completions

hx supports shell completions for bash, zsh, and fish:

```bash
# Bash
hx completions bash > ~/.local/share/bash-completion/completions/hx

# Zsh
hx completions zsh > ~/.zfunc/_hx

# Fish
hx completions fish > ~/.config/fish/completions/hx.fish
```
