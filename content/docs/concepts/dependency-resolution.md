+++
title = "Dependency Resolution"
weight = 2
+++

Understanding how hx manages dependencies.

## Overview

hx manages dependencies through:
1. **Declaration** — Dependencies declared in `.cabal` files
2. **Resolution** — Finding compatible versions
3. **Locking** — Pinning versions in `hx.lock`
4. **Installation** — Downloading and building packages

## Dependency Declaration

### In .cabal Files

Dependencies are declared with version constraints:

```cabal
build-depends:
  , base ^>=4.18
  , text >=2.0 && <3
  , aeson ^>=2.1
  , containers
```

### Version Constraint Syntax

| Syntax | Meaning |
|--------|---------|
| `pkg` | Any version |
| `pkg ==1.2.3` | Exact version |
| `pkg >=1.0` | Minimum version |
| `pkg <2.0` | Maximum version |
| `pkg >=1.0 && <2.0` | Range |
| `pkg ^>=1.2` | Major version (>=1.2 && <1.3) |
| `pkg ^>=1.2.3` | Minor version (>=1.2.3 && <1.3) |

### Adding Dependencies

Use `hx add`:

```bash
hx add text
hx add "aeson ^>=2.1"
```

## Resolution

### How Resolution Works

When you run `hx build` or `hx lock`:

1. **Read constraints** from all `.cabal` files
2. **Query package index** (Hackage)
3. **Run solver** to find compatible versions
4. **Verify** all constraints are satisfied

### The Solver

hx uses Cabal's dependency solver:

- Finds newest compatible versions
- Respects all constraints
- Minimizes dependency footprint
- Handles conflicts

### Solver Failures

When resolution fails:

```
error: Could not resolve dependencies:
  aeson-2.0 requires text >=1.0 && <2
  my-project requires text >=2.0

fix: Adjust version constraints or update dependencies
```

Solutions:
- Update package with conflict: `hx update aeson`
- Relax constraints in `.cabal`
- Use `--allow-newer` (escape hatch)

## Locking

### Purpose

The lockfile ensures reproducible builds:
- Same versions on all machines
- Same versions over time
- Explicit updates

### Creating a Lockfile

```bash
hx lock
```

This creates `hx.lock`:

```toml
version = 1
generated = "2024-01-15T10:30:00Z"

[index]
hackage = "2024-01-15T00:00:00Z"

[[package]]
name = "aeson"
version = "2.1.2.1"
sha256 = "abc123..."

[[package]]
name = "text"
version = "2.0.2"
sha256 = "def456..."
```

### Updating the Lockfile

```bash
# Update all dependencies
hx lock --update

# Update specific packages
hx update aeson
hx lock
```

### Using the Lockfile

```bash
# Install locked versions
hx sync

# Build uses locked versions automatically
hx build
```

## Package Sources

### Hackage

Default package source:

```toml
[[package]]
name = "aeson"
version = "2.1.2.1"
source = "hackage"
```

### Git Repositories

For packages not on Hackage:

In `cabal.project`:

```cabal
source-repository-package
  type: git
  location: https://github.com/user/package
  tag: v1.0.0
```

In `hx.lock`:

```toml
[[package]]
name = "custom-package"
version = "1.0.0"
source = "git"
git = "https://github.com/user/package"
commit = "abc123..."
```

### Local Packages

For local development:

In `cabal.project`:

```cabal
packages:
  .
  ../local-dep
```

## Index State

Pin the Hackage index for reproducibility:

```toml
# hx.toml
[dependencies]
index-state = "2024-01-15T00:00:00Z"
```

This ensures:
- Same packages available everywhere
- No surprises from new uploads
- Reproducible resolution

## Dependency Types

### Regular Dependencies

Required for library and executables:

```cabal
library
  build-depends:
    , base ^>=4.18
    , text ^>=2.0
```

### Test Dependencies

Only for test suites:

```cabal
test-suite my-tests
  build-depends:
    , base ^>=4.18
    , my-lib
    , hspec ^>=2.11     -- test-only
    , QuickCheck ^>=2.14
```

Add with `--dev`:

```bash
hx add --dev hspec QuickCheck
```

### Benchmark Dependencies

Only for benchmarks:

```cabal
benchmark my-bench
  build-depends:
    , base
    , my-lib
    , criterion ^>=1.6
```

### Build Tools

Tools used during build:

```cabal
build-tool-depends:
  , alex
  , happy
```

## Transitive Dependencies

Dependencies have their own dependencies:

```
my-project
└── aeson (direct)
    ├── text (transitive)
    ├── containers (transitive)
    └── vector (transitive)
        └── primitive (transitive)
```

The lockfile includes ALL versions:

```toml
[[package]]
name = "aeson"
version = "2.1.2.1"

[[package]]
name = "text"
version = "2.0.2"

[[package]]
name = "vector"
version = "0.13.0.0"
```

## Version Bounds

### Why Bounds Matter

Upper bounds prevent breakage:

```cabal
-- Good: won't break with aeson-3.0
build-depends: aeson ^>=2.1

-- Risky: will break when aeson API changes
build-depends: aeson >=2.0
```

### PVP (Package Versioning Policy)

Haskell follows PVP:

```
A.B.C.D
│ │ │ └─ Patch (no API change)
│ │ └─── Minor (backwards compatible)
│ └───── Major (breaking change)
└─────── Major (breaking change)
```

Use `^>=` for automatic PVP bounds:

```cabal
-- Allows 2.1.*, blocks 2.2.*
build-depends: aeson ^>=2.1
```

## Conflict Resolution

### Understanding Conflicts

Conflicts occur when packages require incompatible versions:

```
Package A requires text >=1.0 && <2.0
Package B requires text >=2.0
```

### Resolution Strategies

1. **Update conflicting package**
   ```bash
   hx update package-a
   ```

2. **Use newer versions**
   ```bash
   hx lock --update
   ```

3. **Relax constraints** (in your code)
   ```cabal
   -- If you control the package
   build-depends: text >=1.0 && <3.0
   ```

4. **Use allow-newer** (escape hatch)
   ```cabal
   -- cabal.project
   allow-newer: package-a:text
   ```

## See Also

- [hx add](/docs/commands/add) — Add dependencies
- [hx lock](/docs/commands/lock) — Generate lockfile
- [hx sync](/docs/commands/sync) — Install dependencies
- [Lockfiles](/docs/features/lockfiles) — Feature guide
