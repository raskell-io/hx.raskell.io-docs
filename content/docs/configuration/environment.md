+++
title = "Environment Variables"
weight = 3
+++

Environment variables for configuring hx behavior.

## General Settings

### HX_HOME

Override the hx home directory:

```bash
export HX_HOME="$HOME/.hx"
```

Default:
- Linux: `~/.local/share/hx`
- macOS: `~/Library/Application Support/hx`
- Windows: `%APPDATA%\hx`

### HX_CACHE_DIR

Override the cache directory:

```bash
export HX_CACHE_DIR="$HOME/.cache/hx"
```

Default:
- Linux: `~/.cache/hx`
- macOS: `~/Library/Caches/hx`
- Windows: `%LOCALAPPDATA%\hx\cache`

### HX_CONFIG_DIR

Override the config directory:

```bash
export HX_CONFIG_DIR="$HOME/.config/hx"
```

### HX_LOG

Set logging level:

```bash
export HX_LOG=debug  # trace, debug, info, warn, error
```

### HX_LOG_FORMAT

Set log format:

```bash
export HX_LOG_FORMAT=json  # text, json
```

## Build Settings

### HX_JOBS

Default parallel jobs:

```bash
export HX_JOBS=8
```

Overridden by `--jobs` flag and `[build].jobs` config.

### HX_RELEASE

Build in release mode by default:

```bash
export HX_RELEASE=1
```

### HX_GHC_OPTIONS

Additional GHC options:

```bash
export HX_GHC_OPTIONS="-Wall -Werror"
```

## Toolchain Settings

### HX_GHC

Override GHC executable:

```bash
export HX_GHC=/opt/ghc/9.8.2/bin/ghc
```

### HX_CABAL

Override Cabal executable:

```bash
export HX_CABAL=/opt/cabal/3.10/bin/cabal
```

### HX_BHC

Override BHC executable:

```bash
export HX_BHC=/opt/bhc/0.2.0/bin/bhc
```

### HX_GHCUP

Override GHCup executable:

```bash
export HX_GHCUP=/usr/local/bin/ghcup
```

## Output Settings

### NO_COLOR

Disable colored output:

```bash
export NO_COLOR=1
```

### CLICOLOR

Enable/disable color:

```bash
export CLICOLOR=1     # Enable
export CLICOLOR=0     # Disable
```

### CLICOLOR_FORCE

Force colored output (even in non-TTY):

```bash
export CLICOLOR_FORCE=1
```

### HX_COLOR

Control color output:

```bash
export HX_COLOR=always  # always, auto, never
```

### HX_PROGRESS

Show/hide progress indicators:

```bash
export HX_PROGRESS=1   # Show progress
export HX_PROGRESS=0   # Hide progress
```

## Network Settings

### HX_OFFLINE

Offline mode (no network requests):

```bash
export HX_OFFLINE=1
```

### HTTP_PROXY / HTTPS_PROXY

Proxy settings:

```bash
export HTTP_PROXY=http://proxy.example.com:8080
export HTTPS_PROXY=http://proxy.example.com:8080
```

### HX_HACKAGE_SERVER

Override Hackage server:

```bash
export HX_HACKAGE_SERVER=https://hackage.haskell.org
```

## CI Detection

### CI

hx detects CI environments:

```bash
export CI=true
```

In CI mode:
- Progress indicators are simplified
- Prompts are disabled
- Timestamps are added to output

### GITHUB_ACTIONS

Detected automatically:

```yaml
# GitHub Actions - auto-detected
- run: hx build
```

## Path Settings

### PATH

Ensure toolchains are in PATH:

```bash
export PATH="$HOME/.ghcup/bin:$PATH"
export PATH="$HOME/.bhc/bin:$PATH"
```

## Debugging

### HX_DEBUG

Enable debug mode:

```bash
export HX_DEBUG=1
```

Shows:
- Full command invocations
- Timing information
- Detailed error messages

### HX_TRACE

Enable tracing (very verbose):

```bash
export HX_TRACE=1
```

### RUST_BACKTRACE

Show Rust backtraces on panic:

```bash
export RUST_BACKTRACE=1
```

## Shell Configuration

### Bash

Add to `~/.bashrc`:

```bash
# hx configuration
export PATH="$HOME/.ghcup/bin:$PATH"
export HX_LOG=info

# Shell completions
eval "$(hx completions bash)"
```

### Zsh

Add to `~/.zshrc`:

```zsh
# hx configuration
export PATH="$HOME/.ghcup/bin:$PATH"
export HX_LOG=info

# Shell completions
eval "$(hx completions zsh)"
```

### Fish

Add to `~/.config/fish/config.fish`:

```fish
# hx configuration
set -gx PATH $HOME/.ghcup/bin $PATH
set -gx HX_LOG info

# Shell completions
hx completions fish | source
```

## Docker/Container Usage

```dockerfile
FROM haskell:9.8

# Install hx
RUN curl -fsSL https://get.raskell.io/hx | sh

# Configure for container environment
ENV HX_LOG=info
ENV HX_PROGRESS=0
ENV CI=true
```

## See Also

- [Installation](/docs/installation) — Setup guide
- [hx.toml Reference](/docs/configuration/hx-toml) — Configuration file
