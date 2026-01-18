+++
title = "Build System"
weight = 3
+++

Understanding how hx builds projects.

## Overview

hx wraps Cabal's build system, providing:
- Unified interface for builds
- Enhanced error messages
- Progress reporting
- Multiple compiler backends

## Build Process

### What Happens During `hx build`

1. **Configuration**
   - Read `hx.toml` and `.cabal` files
   - Determine compiler and options
   - Check lockfile

2. **Dependency Resolution**
   - Verify dependencies are installed
   - Install missing packages from lockfile

3. **Compilation**
   - Compile modules in dependency order
   - Generate interface files (`.hi`)
   - Generate object files (`.o`)

4. **Linking**
   - Link object files into executables
   - Apply optimizations

### Build Order

Modules are compiled in dependency order:

```
MyProject.Types       (no imports)
MyProject.Parser      (imports Types)
MyProject             (imports Parser, Types)
Main                  (imports MyProject)
```

hx parallelizes independent modules.

## Incremental Builds

### How Incremental Builds Work

Only recompile what changed:

1. **Check timestamps** - Has source file changed?
2. **Check interface hashes** - Have dependencies changed?
3. **Recompile if needed** - Only affected modules

```bash
# First build - compiles everything
$ hx build
Compiling MyProject.Types
Compiling MyProject.Parser
Compiling MyProject
Compiling Main
Linking my-project

# Second build - nothing changed
$ hx build
Up to date

# After editing MyProject.hs
$ hx build
Compiling MyProject  # Only this module
Linking my-project   # Must relink
```

### When to Clean

Force full rebuild:

```bash
hx clean
hx build
```

Reasons to clean:
- Corrupted build state
- Switching GHC versions
- Strange build errors

## Build Artifacts

### Location

Artifacts go to `dist-newstyle/`:

```
dist-newstyle/
├── build/
│   └── x86_64-osx/
│       └── ghc-9.8.2/
│           └── my-project-0.1.0/
│               ├── build/
│               │   ├── MyProject/         # Library objects
│               │   └── my-project/        # Executable
│               │       └── my-project     # The binary
│               └── cache/
├── packagedb/
└── tmp/
```

### Interface Files (.hi)

GHC interface files contain:
- Type signatures
- Exports
- Inlining info

Used for:
- Incremental compilation
- Cross-module optimization

### Object Files (.o)

Compiled machine code, linked into final executable.

## Optimization Levels

### Debug (Default)

```bash
hx build
# Equivalent to: -O0 or -O1
```

Fast compilation, slower runtime.

### Release

```bash
hx build --release
# Equivalent to: -O2
```

Slower compilation, faster runtime.

### Configuration

```toml
[build]
# Default to release builds
release = true

# Or customize optimization
ghc-options = ["-O1"]
```

### Optimization Flags

| Flag | Description |
|------|-------------|
| `-O0` | No optimization |
| `-O1` | Light optimization (default) |
| `-O2` | Full optimization |
| `-fllvm` | Use LLVM backend |
| `-fspecialize` | Specialize functions |

## Parallelism

### Parallel Compilation

```bash
hx build -j8    # 8 parallel jobs
hx build -j0    # Use all CPUs
```

### Configuration

```toml
[build]
jobs = 4  # Default parallel jobs
```

### How It Works

Independent modules compile in parallel:

```
Time →

Thread 1: [Types] → [Parser] → [Main]
Thread 2: [Utils] → [Config]
Thread 3: [Logging]
                    ↓
               [Linking]
```

## Compiler Backends

### GHC (Default)

```bash
hx build --backend ghc
```

Uses `cabal build` under the hood.

### BHC

```bash
hx build --backend bhc
```

Invokes `bhc build` with translated options.

See [Compiler Backends](/docs/features/compiler-backends) for details.

## Build Modes

### Full Build

```bash
hx build
```

Compiles all components.

### Component-Specific

```bash
hx build --lib       # Library only
hx build --exe app   # Specific executable
hx build --test      # Tests only
```

### Check (No Codegen)

```bash
hx check
```

Type checks without producing binaries. Faster for quick feedback.

## GHC Options

### Adding Options

```toml
[build]
ghc-options = ["-Wall", "-Wextra", "-Werror"]
```

Or per-command:

```bash
hx build --ghc-options="-Werror"
```

### Common Options

| Option | Description |
|--------|-------------|
| `-Wall` | Enable common warnings |
| `-Wextra` | Enable extra warnings |
| `-Werror` | Treat warnings as errors |
| `-O2` | Full optimization |
| `-threaded` | Enable multi-threading |
| `-rtsopts` | Enable RTS options |
| `-with-rtsopts=-N` | Use all CPUs at runtime |

## Cross-Compilation

Build for different platforms:

```bash
hx build --target aarch64-linux-gnu
```

See [Cross-Compilation](/docs/features/cross-compilation) for details.

## CI Builds

### Recommended CI Build

```yaml
- name: Build
  run: |
    hx lock --frozen    # Verify lockfile
    hx build            # Build project
    hx test             # Run tests
```

### Caching

Cache `~/.cabal/` and `dist-newstyle/`:

```yaml
- uses: actions/cache@v3
  with:
    path: |
      ~/.cabal
      dist-newstyle
    key: ${{ runner.os }}-cabal-${{ hashFiles('hx.lock') }}
```

## Troubleshooting

### Build Failures

Read the error message carefully:

```
src/MyLib.hs:10:5: error:
    Not in scope: 'undefined'
    Perhaps you meant 'Data.Undefined' (imported from Data)
```

### Incremental Build Issues

If incremental builds produce wrong results:

```bash
hx clean
hx build
```

### Memory Issues

For large projects:

```bash
# Reduce parallelism
hx build -j2

# Or add GHC options
hx build --ghc-options="+RTS -M4G -RTS"
```

## See Also

- [hx build](/docs/commands/build) — Build command
- [hx clean](/docs/commands/clean) — Clean artifacts
- [hx check](/docs/commands/check) — Type check
