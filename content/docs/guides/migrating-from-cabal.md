+++
title = "Migrating from Cabal"
weight = 3
+++

Adopt hx in an existing Cabal project.

## Overview

Migrating from plain Cabal to hx is straightforward because hx uses Cabal under the hood. You're not replacing Cabal—you're adding a better interface on top.

## Quick Migration

```bash
cd my-cabal-project

# Initialize hx
hx init

# Generate lockfile
hx lock

# Build (uses existing Cabal setup)
hx build
```

## What Changes

| Before (Cabal) | After (hx) |
|----------------|-----------|
| `cabal build` | `hx build` |
| `cabal test` | `hx test` |
| `cabal.project` | Keep + add `hx.toml` |
| `cabal.project.freeze` | `hx.lock` (replaces) |

## What Stays the Same

- `*.cabal` files — No changes needed
- `cabal.project` — Keep all settings
- Source files — No changes
- Build output — Same location

## Step-by-Step Migration

### 1. Initialize hx

```bash
cd my-project
hx init --detect
```

This creates `hx.toml`:

```toml
[project]
name = "my-project"
version = "0.1.0"

[toolchain]
ghc = "9.8.2"  # Detected from environment

[build]
ghc-options = ["-Wall"]
```

### 2. Handle cabal.project

If you have a `cabal.project`, hx respects it:

```cabal
-- cabal.project
packages: .

-- hx reads these settings
ghc-options: -Wall
```

You can optionally migrate settings to `hx.toml`:

```toml
# hx.toml
[build]
ghc-options = ["-Wall"]
```

### 3. Handle cabal.project.freeze

If you have a freeze file:

```bash
# hx can import it
hx lock

# Or keep using Cabal freeze format
# Configure in hx.toml:
# [dependencies]
# lock-strategy = "cabal-freeze"
```

### 4. Generate Lockfile

```bash
hx lock
```

Creates `hx.lock` with all pinned versions.

### 5. Verify Everything Works

```bash
hx build
hx test
hx check
```

## Keeping Compatibility

### For Teams Not Ready for hx

Keep both working:

```bash
# Team members using Cabal
cabal build

# Team members using hx
hx build
```

Both produce the same output.

### Gradual Adoption

1. Start with `hx init` — no behavior change
2. Add `hx.lock` — reproducible builds
3. Use hx commands — better UX
4. Eventually, primary tool

## Configuration Mapping

### Package Options

```cabal
-- cabal.project
package my-lib
  ghc-options: -O2
  flags: +feature
```

```toml
# hx.toml (for common cases)
[build]
ghc-options = ["-O2"]
```

Note: Keep complex package-specific options in `cabal.project`.

### Source Repository Packages

Keep in `cabal.project`:

```cabal
source-repository-package
  type: git
  location: https://github.com/user/package
  tag: v1.0.0
```

hx reads these automatically.

### Allow-Newer

Keep in `cabal.project`:

```cabal
allow-newer: some-package:base
```

## CI Migration

### Before (Cabal)

```yaml
- run: cabal update
- run: cabal build
- run: cabal test
```

### After (hx)

```yaml
- run: hx toolchain install
- run: hx lock --frozen
- run: hx build
- run: hx test
```

## Benefits

After migration:

1. **Lockfile** — Reproducible builds
2. **Better errors** — Clearer diagnostics
3. **Watch mode** — `hx watch test`
4. **Integrated formatting** — `hx fmt`
5. **Health checks** — `hx doctor`
6. **Toolchain management** — `hx toolchain install`

## Troubleshooting

### Different Build Results

hx uses the same Cabal backend, so builds should be identical. If different:

1. Check GHC versions match
2. Check lockfile is synced: `hx sync`
3. Clean and rebuild: `hx clean && hx build`

### Missing Dependencies

```bash
# Sync dependencies from lockfile
hx sync
```

### cabal.project Settings Not Respected

hx respects `cabal.project`. Check:

```bash
# Verbose output shows what's being used
hx build --verbose
```

## See Also

- [hx init](/docs/commands/init) — Initialize hx
- [Configuration](/docs/configuration/hx-toml) — hx.toml reference
- [Lockfiles](/docs/features/lockfiles) — Reproducible builds
