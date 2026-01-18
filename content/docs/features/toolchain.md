+++
title = "Toolchain Management"
weight = 3
+++

hx provides integrated management of Haskell toolchains including GHC, Cabal, and BHC.

## Overview

hx can:
- Install multiple compiler versions
- Switch between versions per-project
- Pin toolchain versions in configuration
- Auto-detect required versions
- Download directly or use GHCup

## Supported Tools

| Tool | Description |
|------|-------------|
| **GHC** | Glasgow Haskell Compiler |
| **Cabal** | Build tool and package manager |
| **BHC** | Basel Haskell Compiler |
| **GHCup** | Haskell toolchain installer |
| **HLS** | Haskell Language Server |

## Installing Tools

### Recommended Versions

Install the recommended toolchain:

```bash
hx toolchain install
```

This installs:
- GHC 9.8.2 (recommended)
- Cabal 3.10.3.0 (recommended)

### Specific Versions

```bash
# Install specific GHC version
hx toolchain install --ghc 9.8.2

# Install specific Cabal version
hx toolchain install --cabal 3.10.3.0

# Install both
hx toolchain install --ghc 9.8.2 --cabal 3.10.3.0

# Install BHC
hx toolchain install --bhc 0.2.0
```

### Available Versions

See what versions are available:

```bash
# Available GHC versions
hx toolchain available --ghc

# Available Cabal versions
hx toolchain available --cabal

# Available BHC versions
hx toolchain available --bhc
```

## Managing Installed Tools

### List Installed

```bash
hx toolchain list
```

Output:
```
GHC versions:
  9.8.2 (active)
  9.6.4
  9.4.8

Cabal versions:
  3.10.3.0 (active)
  3.8.1.0

BHC versions:
  0.2.0 (active)
```

### Set Active Version

```bash
# Set active GHC
hx toolchain use --ghc 9.8.2

# Set active Cabal
hx toolchain use --cabal 3.10.3.0

# Set active BHC
hx toolchain use --bhc 0.2.0
```

### Remove Version

```bash
# Remove specific version
hx toolchain remove --ghc 9.6.4

# Remove BHC version
hx toolchain remove --bhc 0.1.0
```

## Per-Project Toolchains

### Configuration

Pin toolchain versions in `hx.toml`:

```toml
[toolchain]
ghc = "9.8.2"
cabal = "3.10.3.0"
```

When you run `hx build` in this project, hx will:
1. Check if the required GHC is installed
2. Use that version regardless of global active version
3. Prompt to install if missing

### Auto-Install

If a required version isn't installed:

```bash
$ hx build
GHC 9.8.2 required but not installed.
Install now? [Y/n] y
Installing GHC 9.8.2...
Building...
```

Or install in advance:

```bash
hx toolchain install --ghc 9.8.2
```

### Override

Override project settings temporarily:

```bash
# Use different GHC for this build
HX_GHC=/path/to/ghc-9.10 hx build
```

## Installation Methods

### Via GHCup (Default)

hx uses GHCup when available:

```bash
hx toolchain install --ghc 9.8.2
# Runs: ghcup install ghc 9.8.2
```

Benefits:
- Shared installation with other tools
- Well-tested binaries
- Automatic PATH management

### Direct Download

Download binaries directly:

```bash
hx toolchain install --ghc 9.8.2 --direct
```

Benefits:
- Works without GHCup
- Faster in some cases
- Custom installation paths

Installation locations:
- GHC: `~/.hx/toolchains/ghc-<version>/`
- Cabal: `~/.hx/toolchains/cabal-<version>/`
- BHC: `~/.bhc/versions/<version>/`

## Version Resolution

hx resolves toolchain versions in this order:

1. **Command-line flags**: `--ghc 9.8.2`
2. **Environment variables**: `HX_GHC`
3. **Project config**: `[toolchain].ghc` in `hx.toml`
4. **Global active**: Set via `hx toolchain use`
5. **System PATH**: Whatever `ghc` is in PATH

## Stackage Resolvers

Use Stackage LTS or Nightly resolvers:

```toml
[toolchain]
resolver = "lts-22.0"
```

This pins to the GHC version used by that resolver.

## Checking Toolchain Health

Verify your toolchain setup:

```bash
hx doctor
```

Output:
```
✓ GHC 9.8.2 installed
✓ Cabal 3.10.3.0 installed
✓ GHCup 0.1.22.0 available
✓ HLS 2.6.0.0 installed
✓ BHC 0.2.0 installed
```

## Platform-Specific Notes

### Linux

GHC binaries require:
- libc (glibc or musl)
- libgmp
- libncurses
- libtinfo

Install on Ubuntu/Debian:
```bash
sudo apt install libgmp-dev libncurses-dev libtinfo-dev
```

### macOS

Use Homebrew for dependencies:
```bash
brew install gmp
```

Apple Silicon Macs can run both ARM64 and x86_64 (via Rosetta) GHC versions.

### Windows

Install via:
- GHCup for Windows
- Chocolatey: `choco install ghc cabal`

## Environment Variables

| Variable | Description |
|----------|-------------|
| `HX_GHC` | Override GHC path |
| `HX_CABAL` | Override Cabal path |
| `HX_BHC` | Override BHC path |
| `HX_GHCUP` | Override GHCup path |

## Best Practices

### 1. Pin Versions in hx.toml

```toml
[toolchain]
ghc = "9.8.2"
```

Ensures everyone on the team uses the same version.

### 2. Use hx doctor

```bash
hx doctor
```

Catches mismatches before they cause problems.

### 3. Update Regularly

```bash
hx toolchain available --ghc
hx toolchain install --ghc 9.10.1
```

### 4. Clean Up Old Versions

```bash
hx toolchain list
hx toolchain remove --ghc 9.4.8
```

## See Also

- [hx toolchain](/docs/commands/toolchain) — Command reference
- [hx doctor](/docs/commands/doctor) — Diagnose issues
- [Installation](/docs/installation) — Initial setup
