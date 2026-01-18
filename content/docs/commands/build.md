+++
title = "hx build"
weight = 1
+++

Compile the project.

## Synopsis

```bash
hx build [OPTIONS]
```

## Description

The `build` command compiles your Haskell project. By default, it builds all components (library, executables, tests, and benchmarks).

hx wraps Cabal's build system, providing a streamlined interface with better error messages and progress reporting.

## Options

### Build Options

```
    --release           Build with optimizations (-O2)
-j, --jobs <N>          Number of parallel jobs (default: number of CPUs)
    --lib               Build only the library
    --exe <NAME>        Build only the specified executable
    --test              Build only tests
    --bench             Build only benchmarks
    --all               Build all components
```

### Compiler Options

```
    --backend <BACKEND>    Compiler backend to use [ghc, bhc]
    --ghc-options <OPTS>   Additional options to pass to GHC
    --target <TARGET>      Cross-compilation target triple
```

### Output Options

```
-v, --verbose           Show full compiler output
    --timings           Show compilation timing information
    --progress          Show progress bar (default in TTY)
```

## Examples

### Basic Build

```bash
# Build all components with default settings
hx build
```

### Release Build

```bash
# Build with optimizations
hx build --release
```

### Parallel Build

```bash
# Use 8 parallel jobs
hx build -j8

# Use all available CPUs
hx build -j0
```

### Build Specific Components

```bash
# Build only the library
hx build --lib

# Build a specific executable
hx build --exe my-app

# Build only tests (without running)
hx build --test
```

### Using Different Compiler Backends

```bash
# Build with GHC (default)
hx build --backend ghc

# Build with BHC (Basel Haskell Compiler)
hx build --backend bhc
```

### Cross-Compilation

```bash
# Build for Linux ARM64
hx build --target aarch64-linux-gnu

# Build for WebAssembly
hx build --target wasm32-wasi --backend bhc
```

### Custom GHC Options

```bash
# Enable warnings as errors
hx build --ghc-options="-Werror"

# Enable specific extensions
hx build --ghc-options="-XOverloadedStrings -XDeriveGeneric"
```

## Configuration

Build behavior can be configured in `hx.toml`:

```toml
[build]
release = false              # Default to debug builds
jobs = 4                     # Parallel jobs
ghc-options = ["-Wall"]      # Default GHC options
split-sections = true        # Enable split sections (smaller binaries)

[compiler]
backend = "ghc"              # Default compiler backend
```

See [Configuration Reference](/docs/configuration/hx-toml) for all options.

## Build Artifacts

Build output is placed in the `dist-newstyle` directory by default. The exact location of binaries depends on your project configuration:

```
dist-newstyle/
└── build/
    └── <platform>/
        └── ghc-<version>/
            └── <package>/
                └── x/
                    └── <exe-name>/
                        └── build/
                            └── <exe-name>/
                                └── <exe-name>    # The binary
```

Use `hx run` to execute binaries without navigating this structure.

## Incremental Builds

hx uses Cabal's incremental build system. Only changed modules are recompiled. To force a full rebuild:

```bash
hx clean && hx build
```

## See Also

- [hx run](/docs/commands/run) — Build and run the executable
- [hx check](/docs/commands/check) — Type check without full compilation
- [hx clean](/docs/commands/clean) — Remove build artifacts
- [Compiler Backends](/docs/features/compiler-backends) — Using GHC vs BHC
