+++
title = "Installation"
weight = 1
+++

## Quick Install

The fastest way to install hx is via the install script:

```bash
curl -fsSL https://get.raskell.io/hx | sh
```

This script will:
1. Detect your operating system and architecture
2. Download the appropriate binary
3. Install it to `~/.local/bin` (or `/usr/local/bin` on macOS)
4. Add the install directory to your PATH if needed

## Platform Support

hx provides pre-built binaries for:

| Platform | Architecture | Status |
|----------|-------------|--------|
| Linux | x86_64 | ✅ Fully supported |
| Linux | aarch64 | ✅ Fully supported |
| macOS | x86_64 (Intel) | ✅ Fully supported |
| macOS | aarch64 (Apple Silicon) | ✅ Fully supported |
| Windows | x86_64 | ✅ Fully supported |

## Alternative Installation Methods

### From GitHub Releases

Download the latest release for your platform from [GitHub Releases](https://github.com/raskell-io/hx/releases).

```bash
# Linux x86_64
curl -LO https://github.com/raskell-io/hx/releases/latest/download/hx-x86_64-unknown-linux-gnu.tar.gz
tar xzf hx-x86_64-unknown-linux-gnu.tar.gz
sudo mv hx /usr/local/bin/

# macOS Apple Silicon
curl -LO https://github.com/raskell-io/hx/releases/latest/download/hx-aarch64-apple-darwin.tar.gz
tar xzf hx-aarch64-apple-darwin.tar.gz
sudo mv hx /usr/local/bin/

# Windows (PowerShell)
Invoke-WebRequest -Uri https://github.com/raskell-io/hx/releases/latest/download/hx-x86_64-pc-windows-msvc.zip -OutFile hx.zip
Expand-Archive hx.zip -DestinationPath .
Move-Item hx.exe $env:LOCALAPPDATA\Programs\hx\
```

### From Source

Building from source requires Rust 1.92.0 or later:

```bash
git clone https://github.com/raskell-io/hx
cd hx
cargo build --release
sudo mv target/release/hx /usr/local/bin/
```

### Using Cargo

```bash
cargo install hx-cli
```

## Verify Installation

After installation, verify hx is working:

```bash
hx --version
```

You should see output like:

```
hx 0.4.0
```

## Shell Completions

hx provides shell completions for bash, zsh, fish, PowerShell, and elvish:

### Bash

```bash
hx completions bash > ~/.local/share/bash-completion/completions/hx
```

Or add to your `~/.bashrc`:

```bash
eval "$(hx completions bash)"
```

### Zsh

```bash
hx completions zsh > ~/.zfunc/_hx
```

Make sure `~/.zfunc` is in your `fpath`. Add to `~/.zshrc`:

```zsh
fpath=(~/.zfunc $fpath)
autoload -Uz compinit && compinit
```

### Fish

```bash
hx completions fish > ~/.config/fish/completions/hx.fish
```

### PowerShell

```powershell
hx completions powershell >> $PROFILE
```

### Elvish

```bash
hx completions elvish > ~/.elvish/lib/hx.elv
```

## Post-Installation Setup

After installing hx, run the doctor command to verify your Haskell environment:

```bash
hx doctor
```

This will check for:
- GHC (Glasgow Haskell Compiler)
- Cabal
- GHCup (optional, for toolchain management)
- HLS (Haskell Language Server)
- System dependencies

If any tools are missing, hx will provide installation instructions.

## Installing the Haskell Toolchain

If you don't have a Haskell toolchain installed, hx can install it for you:

```bash
# Install the recommended GHC version
hx toolchain install

# Or install a specific version
hx toolchain install --ghc 9.8.2

# Install with Cabal
hx toolchain install --ghc 9.8.2 --cabal 3.10.3.0
```

## Updating hx

To update to the latest version:

```bash
# Using the install script
curl -fsSL https://get.raskell.io/hx | sh

# Or if installed via cargo
cargo install hx-cli --force
```

## Uninstalling

To uninstall hx:

```bash
# Remove the binary
rm $(which hx)

# Remove configuration and cache (optional)
rm -rf ~/.config/hx
rm -rf ~/.cache/hx
```
