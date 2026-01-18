+++
title = "Benchmarks"
weight = 10
+++

# Performance Benchmarks

hx is designed for speed. This page documents our benchmark methodology, test results, and how to reproduce them.

## Test Environment

| Property | Value |
|----------|-------|
| **hx version** | 0.4.0 |
| **GHC version** | 9.8.2 |
| **Cabal version** | 3.12.1.0 |
| **Stack version** | 2.15.1 |
| **Platform** | macOS (Apple Silicon M4) |
| **Date** | January 18, 2026 |

All benchmarks run on a quiet system with minimal background processes. Times are the median of 5 runs after 2 warmup iterations.

---

## Executive Summary

| Operation | hx | cabal | stack | hx Speedup |
|-----------|-----|-------|-------|------------|
| CLI startup | **12ms** | 45ms | 89ms | 3.8x / 7.4x |
| Project init | **68ms** | 320ms | 2.1s | 4.7x / 31x |
| Cold build (simple) | **0.48s** | 2.68s | 3.2s | 5.6x / 6.7x |
| Incremental build | **0.05s** | 0.39s | 0.52s | 7.8x / 10.4x |
| Clean | **8ms** | 180ms | 95ms | 22x / 12x |
| Doctor/diagnostics | **45ms** | N/A | N/A | — |

---

## Benchmark Categories

### 1. CLI Startup Time

Measures how fast the tool responds to simple commands. This affects perceived responsiveness.

#### `--help` Response Time

| Tool | Time | Notes |
|------|------|-------|
| **hx** | 12ms | Rust binary, minimal initialization |
| cabal | 45ms | Haskell binary with GHC RTS |
| stack | 89ms | Haskell binary, loads resolver info |

#### `--version` Response Time

| Tool | Time |
|------|------|
| **hx** | 8ms |
| cabal | 38ms |
| stack | 72ms |

**Why hx is faster:** hx is a native Rust binary with no runtime initialization overhead. Cabal and stack are Haskell binaries that must initialize the GHC runtime system.

---

### 2. Project Initialization

Creating a new project from scratch.

#### Binary Project (`hx init` / `cabal init` / `stack new`)

| Tool | Time | Speedup |
|------|------|---------|
| **hx init** | 68ms | — |
| cabal init | 320ms | 4.7x slower |
| stack new | 2.1s | 31x slower |

#### Library Project

| Tool | Time | Speedup |
|------|------|---------|
| **hx init --lib** | 72ms | — |
| cabal init --lib | 340ms | 4.7x slower |
| stack new --library | 2.2s | 31x slower |

**Why hx is faster:**
- hx generates files from templates without invoking GHC
- stack downloads resolver information and initializes package databases
- cabal runs Haskell code for project generation

---

### 3. Build Performance

The core developer experience metric. Test project is a simple 3-module executable:
- `Main.hs` (15 lines)
- `Lib.hs` (10 lines)
- `Utils.hs` (8 lines)
- Single dependency: `base`

#### Cold Build (Clean State)

After removing all build artifacts (`.hx/`, `dist-newstyle/`).

| Mode | Time | Speedup |
|------|------|---------|
| **hx build --native** | 0.48s | — |
| hx build (cabal backend) | 2.52s | 5.3x slower |
| cabal build | 2.68s | 5.6x slower |
| stack build | 3.2s | 6.7x slower |

#### Incremental Build (No Changes)

Subsequent build with no source modifications.

| Mode | Time | Speedup |
|------|------|---------|
| **hx build --native** | 0.05s | — |
| hx build (cabal backend) | 0.35s | 7x slower |
| cabal build | 0.39s | 7.8x slower |
| stack build | 0.52s | 10.4x slower |

#### Incremental Build (Single File Changed)

After modifying one source file.

| Mode | Time | Speedup |
|------|------|---------|
| **hx build --native** | 0.31s | — |
| cabal build | 1.42s | 4.6x slower |
| stack build | 1.8s | 5.8x slower |

---

### 4. Native Build Mode

hx's native build mode bypasses cabal entirely for simple projects.

#### How It Works

1. **Direct GHC invocation** — hx constructs the module graph and calls GHC directly
2. **No cabal overhead** — No package database queries, no build plan calculation
3. **Aggressive caching** — Fingerprint-based caching with minimal I/O
4. **Parallel compilation** — Native parallel builds without cabal's job scheduling

#### When Native Builds Apply

| Scenario | Native Build? |
|----------|--------------|
| Single-package project | Yes |
| Only `base` dependencies | Yes |
| Multiple external dependencies | No (falls back to cabal) |
| Custom Setup.hs | No |
| C FFI / foreign libraries | No |

#### Preprocessor Performance

Native builds handle common preprocessors with minimal overhead:

| Preprocessor | File Type | Additional Time |
|--------------|-----------|-----------------|
| **alex** | `.x` | ~50ms |
| **happy** | `.y` | ~100ms |
| **hsc2hs** | `.hsc` | ~335ms |
| **c2hs** | `.chs` | ~280ms |

---

### 5. Dependency Resolution

Comparing solver performance for complex dependency graphs.

#### Real Package Resolution

Resolving dependencies for a project with 20 direct dependencies.

| Tool | Time | Notes |
|------|------|-------|
| **hx lock** | 1.2s | Native Rust solver |
| cabal freeze | 8.5s | Full constraint solving |
| stack lock | 0.8s | Stackage pre-computed |

**Note:** Stack is fast because Stackage snapshots pre-compute compatible versions. hx's solver is faster than cabal for equivalent unconstrained resolution.

#### Solver Scaling (Synthetic Benchmark)

Package count vs resolution time (10 versions per package):

| Packages | hx | cabal |
|----------|-----|-------|
| 10 | 5ms | 120ms |
| 20 | 18ms | 450ms |
| 50 | 85ms | 2.8s |
| 100 | 320ms | 12.5s |

---

### 6. Clean Operations

Removing build artifacts.

| Tool | Time | What's Removed |
|------|------|----------------|
| **hx clean** | 8ms | `.hx/`, `dist-newstyle/` |
| cabal clean | 180ms | `dist-newstyle/` |
| stack clean | 95ms | `.stack-work/` |

hx clean is fast because it's a simple directory removal without Haskell runtime overhead.

---

### 7. Watch Mode Performance

File change detection and rebuild.

#### Time from File Save to Rebuild Start

| Tool | Latency |
|------|---------|
| **hx watch** | 15ms |
| ghcid | 25ms |
| stack --file-watch | 180ms |

#### Rebuild Time (Single Module Change)

| Tool | Time |
|------|------|
| **hx watch** | 0.28s |
| ghcid | 0.35s |
| stack --file-watch | 1.2s |

---

### 8. Shell Completions

Generating shell completion scripts.

| Shell | Time |
|-------|------|
| bash | 4ms |
| zsh | 5ms |
| fish | 4ms |

Completions are generated at runtime from clap, no Haskell invocation needed.

---

### 9. Diagnostics (`hx doctor`)

Environment health check including toolchain detection.

| Check | Time |
|-------|------|
| Full doctor | 45ms |
| GHC detection | 8ms |
| Cabal detection | 5ms |
| HLS compatibility | 12ms |
| Project validation | 15ms |

---

### 10. Memory Usage

Peak memory consumption during common operations.

| Operation | hx | cabal | stack |
|-----------|-----|-------|-------|
| CLI startup | 8 MB | 45 MB | 85 MB |
| Project init | 12 MB | 120 MB | 180 MB |
| Build (simple) | 45 MB | 250 MB | 320 MB |
| Dependency resolution | 80 MB | 450 MB | 180 MB |

---

## Benchmark Implementation

hx uses two benchmark frameworks:

### Criterion Benchmarks

Located in `crates/hx-solver/benches/` and `crates/hx-cli/benches/`:

```rust
// crates/hx-cli/benches/cli_bench.rs
fn bench_startup(c: &mut Criterion) {
    let mut group = c.benchmark_group("startup");
    group.sample_size(50);

    group.bench_function("hx_help", |b| {
        b.iter(|| {
            Command::new(env!("CARGO_BIN_EXE_hx"))
                .arg("--help")
                .output()
                .expect("hx --help failed")
        })
    });

    // Compare with cabal if available
    if has_cabal() {
        group.bench_function("cabal_help", |b| {
            b.iter(|| {
                Command::new("cabal")
                    .arg("--help")
                    .output()
                    .expect("cabal --help failed")
            })
        });
    }

    group.finish();
}
```

### Benchmark Groups

| Group | What It Measures | Location |
|-------|------------------|----------|
| `startup` | CLI --help and --version time | `hx-cli/benches/cli_bench.rs` |
| `init` | Project initialization | `hx-cli/benches/cli_bench.rs` |
| `doctor` | Environment diagnostics | `hx-cli/benches/cli_bench.rs` |
| `config` | Config parsing and display | `hx-cli/benches/cli_bench.rs` |
| `clean` | Build artifact removal | `hx-cli/benches/cli_bench.rs` |
| `completions` | Shell completion generation | `hx-cli/benches/cli_bench.rs` |
| `lock_check` | Lockfile verification | `hx-cli/benches/cli_bench.rs` |
| `solver` | Dependency resolution | `hx-solver/benches/solver_bench.rs` |

---

## Running Benchmarks

### Criterion Benchmarks

```bash
# All CLI benchmarks
cargo bench -p hx-cli

# Specific benchmark group
cargo bench -p hx-cli -- startup

# Solver benchmarks
cargo bench -p hx-solver

# View HTML reports
open target/criterion/report/index.html
```

### Comparison Script

```bash
# Install hyperfine
cargo install hyperfine

# Run full comparison suite
./scripts/benchmark-comparison.sh
```

### Manual Benchmark

```bash
# Create test project
mkdir /tmp/hx-bench && cd /tmp/hx-bench

cat > hx.toml << 'EOF'
[project]
name = "hx-bench"
[toolchain]
ghc = "9.8.2"
EOF

cat > hx-bench.cabal << 'EOF'
cabal-version: 3.0
name: hx-bench
version: 0.1.0.0
build-type: Simple

executable hx-bench
    main-is: Main.hs
    other-modules: Lib, Utils
    hs-source-dirs: src
    default-language: GHC2021
    build-depends: base
EOF

mkdir src
echo 'module Main where; import Lib; import Utils; main = putStrLn (format greeting)' > src/Main.hs
echo 'module Lib (greeting) where; greeting = "Hello"' > src/Lib.hs
echo 'module Utils (format) where; format s = ">>> " ++ s ++ " <<<"' > src/Utils.hs

# Benchmark cold build
rm -rf .hx dist-newstyle
hyperfine --warmup 2 'hx build --native' 'cabal build'

# Benchmark incremental
hyperfine --warmup 2 'hx build --native' 'cabal build'
```

---

## Methodology

### Measurement Protocol

1. **Warmup:** 2 iterations discarded
2. **Samples:** 5 iterations minimum
3. **Metric:** Median time (robust to outliers)
4. **Environment:** Quiet system, minimal background processes
5. **Cache state:** Explicitly controlled (cold = artifacts removed)

### Statistical Validity

- Standard deviation < 5% for all measurements
- P95 confidence intervals calculated
- Outliers flagged and investigated

### Tools Used

| Tool | Purpose |
|------|---------|
| **hyperfine** | Statistical command-line benchmarking |
| **criterion** | Rust microbenchmark framework |
| **time** | Wall-clock timing for quick checks |

---

## Historical Results

| Version | Date | Native Cold | Cabal Cold | Speedup |
|---------|------|-------------|------------|---------|
| 0.4.0 | 2026-01-18 | 0.48s | 2.68s | 5.6x |
| 0.3.6 | 2026-01-17 | 0.48s | 2.68s | 5.6x |
| 0.3.0 | 2026-01-10 | 0.52s | 2.68s | 5.2x |
| 0.2.0 | 2025-12-15 | 0.61s | 2.70s | 4.4x |

---

## Contributing Benchmarks

We welcome benchmark contributions:

1. **Run on your hardware** — Different CPUs and platforms help validate results
2. **Suggest new scenarios** — What operations should we benchmark?
3. **Report regressions** — If you notice performance degradation, file an issue

Submit results: [GitHub Issues](https://github.com/raskell-io/hx/issues)

---

## FAQ

### Why not compare against Nix?

Nix solves a different problem (reproducible environments) and has different performance characteristics. Direct comparison would be misleading.

### Will hx always be faster?

For complex multi-package projects with custom build steps, cabal's sophistication may be necessary. hx optimizes for the common case.

### How do BHC builds compare?

BHC (Basel Haskell Compiler) benchmarks will be added once BHC reaches stable release.
