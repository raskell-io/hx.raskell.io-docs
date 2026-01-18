+++
title = "hx.toml Reference"
weight = 1
+++

Complete reference for the `hx.toml` configuration file.

## Overview

`hx.toml` is the main configuration file for hx projects. It uses the TOML format and lives in your project root.

## [project]

Project metadata.

```toml
[project]
name = "my-project"           # Package name
version = "0.1.0"             # Package version
description = "My library"    # Short description
license = "MIT"               # SPDX license identifier
authors = ["Your Name <you@example.com>"]
repository = "https://github.com/user/project"
homepage = "https://project.example.com"
```

### Fields

| Field | Type | Description |
|-------|------|-------------|
| `name` | string | Package name (must match .cabal) |
| `version` | string | Semantic version |
| `description` | string | Short description |
| `license` | string | SPDX license identifier |
| `authors` | array | List of authors |
| `repository` | string | Repository URL |
| `homepage` | string | Project homepage URL |

## [toolchain]

Toolchain version pinning.

```toml
[toolchain]
ghc = "9.8.2"                 # GHC version
cabal = "3.10.3.0"            # Cabal version (optional)
resolver = "lts-22.0"         # Stackage resolver (optional)
```

### Fields

| Field | Type | Description |
|-------|------|-------------|
| `ghc` | string | Required GHC version |
| `cabal` | string | Required Cabal version |
| `resolver` | string | Stackage resolver name |

### Examples

```toml
# Pin exact GHC version
[toolchain]
ghc = "9.8.2"

# Pin both GHC and Cabal
[toolchain]
ghc = "9.8.2"
cabal = "3.10.3.0"

# Use Stackage resolver
[toolchain]
resolver = "lts-22.0"
```

## [compiler]

Compiler backend configuration.

```toml
[compiler]
backend = "ghc"               # "ghc" or "bhc"
version = "9.8.2"             # Compiler version (overrides toolchain)
```

### Fields

| Field | Type | Description |
|-------|------|-------------|
| `backend` | string | Compiler backend: `"ghc"` (default) or `"bhc"` |
| `version` | string | Compiler version |

### [compiler.ghc]

GHC-specific configuration.

```toml
[compiler.ghc]
version = "9.8.2"             # GHC version
options = ["-Wall"]           # Default GHC options
```

### [compiler.bhc]

BHC (Basel Haskell Compiler) configuration.

```toml
[compiler.bhc]
profile = "numeric"           # "default", "server", "numeric", "edge"
emit_kernel_report = false    # Generate kernel optimization report
tensor_fusion = false         # Enable tensor fusion optimization
target = "aarch64-linux-gnu"  # Cross-compilation target
```

#### BHC Profiles

| Profile | Description |
|---------|-------------|
| `default` | Balanced for general use |
| `server` | Optimized for server workloads |
| `numeric` | Optimized for numerical computation |
| `edge` | Optimized for edge/embedded devices |

### Examples

```toml
# Use GHC (default)
[compiler]
backend = "ghc"

# Use BHC with numeric optimizations
[compiler]
backend = "bhc"

[compiler.bhc]
profile = "numeric"
tensor_fusion = true
```

## [build]

Build configuration.

```toml
[build]
release = false               # Build with optimizations by default
jobs = 4                      # Parallel jobs (0 = auto)
ghc-options = ["-Wall", "-Wextra"]
split-sections = false        # Enable split sections (smaller binaries)
incremental = true            # Enable incremental builds
```

### Fields

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `release` | bool | `false` | Build with optimizations |
| `jobs` | int | CPU count | Parallel compilation jobs |
| `ghc-options` | array | `[]` | Options passed to GHC |
| `split-sections` | bool | `false` | Enable -split-sections |
| `incremental` | bool | `true` | Enable incremental builds |

### Examples

```toml
# Development defaults
[build]
release = false
ghc-options = ["-Wall", "-Wcompat"]

# Production build
[build]
release = true
ghc-options = ["-Wall", "-Werror", "-O2"]
split-sections = true

# Fast iteration
[build]
jobs = 0
incremental = true
```

## [test]

Test configuration.

```toml
[test]
default-suite = "unit-tests"  # Default test suite to run
fail-fast = false             # Stop on first failure
show-timing = true            # Show test timing
coverage = false              # Generate coverage report
```

### Fields

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `default-suite` | string | all | Default test suite |
| `fail-fast` | bool | `false` | Stop on first failure |
| `show-timing` | bool | `true` | Display timing info |
| `coverage` | bool | `false` | Generate coverage |

### [test.coverage]

Coverage configuration.

```toml
[test.coverage]
enabled = false
output-dir = "coverage"
exclude = ["generated/**"]
```

## [bench]

Benchmark configuration.

```toml
[bench]
default-suite = "criterion"   # Default benchmark suite
release = true                # Always build with optimizations
```

### Fields

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `default-suite` | string | all | Default benchmark suite |
| `release` | bool | `true` | Build with optimizations |

## [run]

Run configuration.

```toml
[run]
default-exe = "my-app"        # Default executable
args = []                     # Default arguments
```

### Fields

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `default-exe` | string | auto | Default executable to run |
| `args` | array | `[]` | Default program arguments |

## [repl]

REPL configuration.

```toml
[repl]
default = "lib"               # Default component: lib, exe:name, test:name
auto-load = ["MyProject"]     # Modules to load on start
ghci-options = ["-XOverloadedStrings"]
ghci-conf = ".ghci"           # Custom .ghci file
```

### Fields

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `default` | string | `"lib"` | Default component to load |
| `auto-load` | array | `[]` | Modules to import |
| `ghci-options` | array | `[]` | GHCi options |
| `ghci-conf` | string | `.ghci` | GHCi config file |

## [doc]

Documentation configuration.

```toml
[doc]
internal = false              # Include internal modules
haddock-options = ["--hyperlinked-source"]
output-dir = "docs"           # Output directory
```

### Fields

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `internal` | bool | `false` | Document internal modules |
| `haddock-options` | array | `[]` | Haddock options |
| `output-dir` | string | default | Documentation output |

## [fmt]

Code formatting configuration.

```toml
[fmt]
formatter = "ormolu"          # "ormolu", "fourmolu", "stylish"
check = false                 # Check only, don't modify
exclude = ["generated/**"]    # Patterns to exclude
```

### Fields

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `formatter` | string | `"ormolu"` | Formatter to use |
| `check` | bool | `false` | Check-only mode |
| `exclude` | array | `[]` | Excluded patterns |

## [lint]

Linter configuration.

```toml
[lint]
extra-hints = [".hlint.yaml"] # Additional hint files
ignore = ["Eta reduce"]       # Ignored hints
```

### Fields

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `extra-hints` | array | `[]` | Additional hint files |
| `ignore` | array | `[]` | Ignored hints |

## [watch]

Watch mode configuration.

```toml
[watch]
clear = true                  # Clear terminal on rebuild
debounce = 300                # Debounce delay (ms)
include = ["config/**"]       # Additional patterns to watch
exclude = ["dist-newstyle/**"] # Patterns to exclude
```

### Fields

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `clear` | bool | `false` | Clear terminal |
| `debounce` | int | `300` | Debounce delay in ms |
| `include` | array | `[]` | Additional watch patterns |
| `exclude` | array | `[]` | Excluded patterns |

## [dependencies]

Dependency management configuration.

```toml
[dependencies]
lock-strategy = "hx"          # "hx" or "cabal-freeze"
index-state = "2024-01-15T00:00:00Z"
```

### Fields

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `lock-strategy` | string | `"hx"` | Lockfile strategy |
| `index-state` | string | none | Hackage index state |

## Complete Example

```toml
[project]
name = "my-app"
version = "1.0.0"
description = "My awesome Haskell application"
license = "MIT"
authors = ["Developer <dev@example.com>"]
repository = "https://github.com/example/my-app"

[toolchain]
ghc = "9.8.2"
cabal = "3.10.3.0"

[compiler]
backend = "ghc"

[compiler.ghc]
options = ["-Wall", "-Wextra", "-Wcompat"]

[build]
release = false
jobs = 4
split-sections = true

[test]
fail-fast = true
show-timing = true

[test.coverage]
enabled = true
output-dir = "coverage"

[run]
default-exe = "my-app"
args = ["--verbose"]

[repl]
auto-load = ["MyApp", "MyApp.Config"]
ghci-options = ["-XOverloadedStrings"]

[fmt]
formatter = "fourmolu"
exclude = ["generated/**"]

[lint]
ignore = ["Use camelCase"]

[watch]
clear = true
debounce = 200
exclude = [".git/**", "dist-newstyle/**"]

[dependencies]
lock-strategy = "hx"
```
