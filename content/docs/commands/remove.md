+++
title = "hx remove"
weight = 13
+++

Remove dependencies from your project.

## Synopsis

```bash
hx remove <PACKAGES>... [OPTIONS]
```

## Description

The `remove` command removes one or more package dependencies from your project. It updates your `.cabal` file and optionally updates the lockfile.

## Arguments

```
PACKAGES...             Package names to remove
```

## Options

```
    --all               Remove from all components
    --lib               Remove from library only
    --exe <NAME>        Remove from specific executable
    --test <NAME>       Remove from specific test suite
    --bench <NAME>      Remove from specific benchmark
    --no-lock           Don't update lockfile
-v, --verbose           Show detailed output
```

## Examples

### Remove Single Package

```bash
hx remove text
```

### Remove Multiple Packages

```bash
hx remove text containers aeson
```

### Remove from All Components

```bash
hx remove --all text
```

### Remove from Specific Component

```bash
# Remove from library only
hx remove --lib text

# Remove from specific executable
hx remove --exe my-cli optparse-applicative

# Remove from test suite
hx remove --test unit-tests hspec
```

## What It Does

1. **Removes from .cabal file**
   - Updates build-depends for specified components
   - Preserves other configuration

2. **Updates lockfile** (unless `--no-lock`)
   - Removes unused transitive dependencies

### Before

```cabal
library
  build-depends:
    , base ^>=4.18
    , containers
    , text
```

### After `hx remove text`

```cabal
library
  build-depends:
    , base ^>=4.18
    , containers
```

## Safety Checks

hx warns if:
- Package is still used in source files
- Other packages depend on it
- Package is not found in dependencies

```bash
$ hx remove text
warning: 'text' may still be used in:
  - src/MyLib.hs (line 5)
Continue anyway? [y/N]
```

Use `--force` to skip confirmation:

```bash
hx remove --force text
```

## Cleanup

After removing dependencies, you may want to:

```bash
# Update lockfile
hx lock

# Clean build artifacts
hx clean

# Rebuild
hx build
```

## See Also

- [hx add](/docs/commands/add) — Add dependencies
- [hx update](/docs/commands/update) — Update dependencies
- [hx outdated](/docs/commands/outdated) — Check for outdated dependencies
