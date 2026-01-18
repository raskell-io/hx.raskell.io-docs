+++
title = "hx init"
weight = 11
+++

Initialize hx in an existing project.

## Synopsis

```bash
hx init [OPTIONS]
```

## Description

The `init` command initializes hx in an existing Haskell project by creating an `hx.toml` configuration file. It detects existing Cabal configuration and toolchain settings.

## Options

```
    --force             Overwrite existing hx.toml
    --ghc <VERSION>     Specify GHC version
    --backend <BACKEND> Compiler backend [ghc, bhc]
    --detect            Auto-detect settings from existing configuration
-v, --verbose           Show detailed output
```

## Examples

### Basic Initialization

```bash
cd existing-project
hx init
```

### With Auto-Detection

```bash
# Detect GHC version from cabal.project or stack.yaml
hx init --detect
```

### Force Overwrite

```bash
hx init --force
```

### Specify GHC Version

```bash
hx init --ghc 9.8.2
```

## What It Does

1. **Detects existing configuration**
   - Reads `.cabal` files for package info
   - Checks `cabal.project` for GHC settings
   - Checks `stack.yaml` if present

2. **Creates hx.toml**
   - Generates configuration based on detected settings
   - Sets up toolchain pinning

3. **Does NOT modify**
   - Existing `.cabal` files
   - `cabal.project`
   - `stack.yaml`
   - Source files

## Generated hx.toml

Based on detection:

```toml
[project]
name = "detected-name"
version = "detected-version"

[toolchain]
ghc = "9.8.2"    # Detected or specified

[build]
ghc-options = ["-Wall"]

# If cabal.project exists with freeze file
[dependencies]
lock-strategy = "cabal-freeze"
```

## Detection Sources

hx looks for settings in:

| Source | Detects |
|--------|---------|
| `*.cabal` | Package name, version |
| `cabal.project` | GHC version, packages |
| `cabal.project.freeze` | Pinned dependencies |
| `stack.yaml` | Resolver, extra-deps |
| System PATH | Active GHC version |

## Migrating from Stack

If you have a Stack project:

```bash
cd stack-project
hx init --detect
```

hx will:
- Read resolver from `stack.yaml`
- Map to equivalent GHC version
- Create `hx.toml` with settings

You can then use hx alongside or instead of Stack.

## Migrating from Cabal

If you have a Cabal-only project:

```bash
cd cabal-project
hx init --detect
```

hx will:
- Read settings from `cabal.project`
- Create `hx.toml` with configuration
- Respect existing `cabal.project.freeze`

## After Initialization

After `hx init`:

```bash
# Verify environment
hx doctor

# Build
hx build

# Generate lockfile
hx lock
```

## See Also

- [hx new](/docs/commands/new) — Create new project
- [Configuration Reference](/docs/configuration/hx-toml)
