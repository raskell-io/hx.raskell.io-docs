+++
title = "hx toolchain"
weight = 30
+++

Manage Haskell toolchains (GHC, Cabal, BHC).

## Synopsis

```bash
hx toolchain <SUBCOMMAND> [OPTIONS]
```

## Subcommands

| Subcommand | Description |
|------------|-------------|
| `install` | Install a toolchain version |
| `remove` | Remove an installed version |
| `list` | List installed versions |
| `use` | Set active version |
| `available` | Show available versions |

## hx toolchain install

Install GHC, Cabal, or BHC.

### Synopsis

```bash
hx toolchain install [OPTIONS]
```

### Options

```
    --ghc <VERSION>     GHC version to install
    --cabal <VERSION>   Cabal version to install
    --bhc <VERSION>     BHC version to install
    --recommended       Install recommended versions
    --direct            Download directly (skip ghcup)
-v, --verbose           Show detailed output
```

### Examples

```bash
# Install recommended versions
hx toolchain install

# Install specific GHC version
hx toolchain install --ghc 9.8.2

# Install GHC and Cabal together
hx toolchain install --ghc 9.8.2 --cabal 3.10.3.0

# Install BHC
hx toolchain install --bhc 0.2.0

# Direct download (without ghcup)
hx toolchain install --ghc 9.8.2 --direct
```

## hx toolchain remove

Remove an installed toolchain version.

### Synopsis

```bash
hx toolchain remove [OPTIONS]
```

### Options

```
    --ghc <VERSION>     GHC version to remove
    --cabal <VERSION>   Cabal version to remove
    --bhc <VERSION>     BHC version to remove
    --all               Remove all versions
```

### Examples

```bash
# Remove specific GHC version
hx toolchain remove --ghc 9.6.4

# Remove BHC
hx toolchain remove --bhc 0.1.0
```

## hx toolchain list

List installed toolchain versions.

### Synopsis

```bash
hx toolchain list [OPTIONS]
```

### Options

```
    --ghc               Show only GHC versions
    --cabal             Show only Cabal versions
    --bhc               Show only BHC versions
    --json              Output as JSON
```

### Examples

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

## hx toolchain use

Set the active toolchain version.

### Synopsis

```bash
hx toolchain use [OPTIONS]
```

### Options

```
    --ghc <VERSION>     Set active GHC version
    --cabal <VERSION>   Set active Cabal version
    --bhc <VERSION>     Set active BHC version
```

### Examples

```bash
# Set active GHC
hx toolchain use --ghc 9.8.2

# Set active BHC
hx toolchain use --bhc 0.2.0
```

## hx toolchain available

Show versions available for installation.

### Synopsis

```bash
hx toolchain available [OPTIONS]
```

### Options

```
    --ghc               Show available GHC versions
    --cabal             Show available Cabal versions
    --bhc               Show available BHC versions
    --all               Show all available versions
```

### Examples

```bash
# Show available GHC versions
hx toolchain available --ghc
```

Output:
```
Available GHC versions:
  9.10.1
  9.8.2 (recommended)
  9.8.1
  9.6.5
  9.6.4
  ...
```

## Installation Methods

hx can install toolchains via:

### GHCup (Default)

Uses ghcup if available:
```bash
hx toolchain install --ghc 9.8.2
# Runs: ghcup install ghc 9.8.2
```

### Direct Download

Downloads binaries directly:
```bash
hx toolchain install --ghc 9.8.2 --direct
```

Benefits:
- Works without ghcup
- Faster in some cases
- Supports custom install paths

## Platform Support

### GHC Platforms

| Platform | Architecture | Status |
|----------|-------------|--------|
| Linux | x86_64 | ✓ |
| Linux | aarch64 | ✓ |
| macOS | x86_64 | ✓ |
| macOS | aarch64 | ✓ |
| Windows | x86_64 | ✓ |

### BHC Platforms

| Platform | Architecture | Status |
|----------|-------------|--------|
| Linux | x86_64 | ✓ |
| Linux | aarch64 | ✓ |
| macOS | x86_64 | ✓ |
| macOS | aarch64 | ✓ |

## Installation Paths

### GHCup Managed

```
~/.ghcup/ghc/<version>/
~/.ghcup/cabal/<version>/
```

### Direct Install

```
~/.hx/toolchains/ghc-<version>/
~/.hx/toolchains/cabal-<version>/
~/.bhc/versions/<version>/
```

## Project Toolchain

Pin toolchain in `hx.toml`:

```toml
[toolchain]
ghc = "9.8.2"
cabal = "3.10.3.0"
```

hx automatically uses the correct version for each project.

## See Also

- [Toolchain Management](/docs/features/toolchain) — Feature guide
- [hx doctor](/docs/commands/doctor) — Check toolchain health
