+++
title = "hx add"
weight = 12
+++

Add dependencies to your project.

## Synopsis

```bash
hx add <PACKAGES>... [OPTIONS]
```

## Description

The `add` command adds one or more package dependencies to your project. It updates your `.cabal` file and optionally updates the lockfile.

## Arguments

```
PACKAGES...             Package names (with optional version constraints)
```

## Options

```
    --dev               Add as development dependency (test/bench only)
    --lib               Add to library dependencies
    --exe <NAME>        Add to specific executable
    --test <NAME>       Add to specific test suite
    --bench <NAME>      Add to specific benchmark
    --no-lock           Don't update lockfile
-v, --verbose           Show detailed output
```

## Examples

### Add Single Package

```bash
hx add text
```

### Add Multiple Packages

```bash
hx add text containers aeson bytestring
```

### Add with Version Constraint

```bash
# Exact version
hx add "aeson ==2.1.0.0"

# Version range
hx add "aeson >=2.0 && <2.2"

# Major version bound
hx add "aeson ^>=2.1"
```

### Add as Dev Dependency

```bash
# Added to test-suite dependencies only
hx add --dev hspec QuickCheck

# Added to benchmark dependencies only
hx add --bench criterion
```

### Add to Specific Component

```bash
# Add to library
hx add --lib text

# Add to specific executable
hx add --exe my-cli optparse-applicative

# Add to specific test suite
hx add --test unit-tests hspec
```

## Version Constraints

hx supports Cabal version constraint syntax:

| Constraint | Meaning |
|------------|---------|
| `aeson` | Any version |
| `aeson ==2.1.0.0` | Exact version |
| `aeson >=2.0` | Minimum version |
| `aeson <3.0` | Maximum version |
| `aeson >=2.0 && <3.0` | Version range |
| `aeson ^>=2.1` | Major version (>=2.1 && <2.2) |

## What It Does

1. **Parses package specifications**
2. **Validates package exists** on Hackage
3. **Updates .cabal file** with new dependencies
4. **Updates lockfile** (unless `--no-lock`)

### Before

```cabal
library
  build-depends:
    , base ^>=4.18
```

### After `hx add text containers`

```cabal
library
  build-depends:
    , base ^>=4.18
    , containers
    , text
```

## Development Dependencies

Development dependencies are only needed for testing or benchmarking:

```bash
# Test dependencies
hx add --dev hspec QuickCheck hedgehog tasty

# Benchmark dependencies
hx add --dev criterion gauge tasty-bench
```

These are added to the appropriate component:

```cabal
test-suite my-tests
  build-depends:
    , base
    , my-lib
    , hspec        -- Added by --dev
    , QuickCheck   -- Added by --dev
```

## Package Discovery

If you're not sure of the exact package name:

```bash
# Search on Hackage
hx search json

# Or browse https://hackage.haskell.org
```

## Common Packages

| Package | Purpose |
|---------|---------|
| `text` | Efficient Unicode text |
| `bytestring` | Efficient byte strings |
| `containers` | Maps, Sets, Sequences |
| `aeson` | JSON parsing/encoding |
| `http-client` | HTTP client |
| `optparse-applicative` | CLI argument parsing |
| `mtl` | Monad transformers |
| `lens` | Lenses and optics |
| `vector` | Efficient arrays |
| `async` | Async/concurrent programming |

## Lockfile Behavior

By default, `hx add` updates the lockfile:

```bash
# Add and update lockfile
hx add text

# Add without updating lockfile
hx add text --no-lock
```

To update the lockfile later:

```bash
hx lock
```

## See Also

- [hx remove](/docs/commands/remove) — Remove dependencies
- [hx update](/docs/commands/update) — Update dependencies
- [hx lock](/docs/commands/lock) — Update lockfile
