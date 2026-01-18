+++
title = "hx bench"
weight = 4
+++

Run benchmarks.

## Synopsis

```bash
hx bench [OPTIONS] [-- <ARGS>...]
```

## Description

The `bench` command builds and runs your project's benchmarks. It supports common benchmarking frameworks like Criterion, Gauge, and Tasty-Bench.

## Options

```
    --bench <NAME>      Run a specific benchmark suite
    --match <PATTERN>   Run benchmarks matching pattern
    --no-run            Build benchmarks without running
    --output <FILE>     Write results to file
    --release           Build with optimizations (recommended)
    --backend <BACKEND> Compiler backend [ghc, bhc]
-v, --verbose           Show detailed output
```

## Examples

### Run All Benchmarks

```bash
# Build and run all benchmark suites
hx bench
```

### Release Mode (Recommended)

```bash
# Always use release mode for accurate benchmarks
hx bench --release
```

### Run Specific Benchmark Suite

```bash
hx bench --bench my-benchmarks
```

### Filter Benchmarks

```bash
# Run benchmarks matching a pattern
hx bench --match "Parser"
```

### Output to File

```bash
# Save results to CSV
hx bench -- --output bench-results.csv

# Save Criterion HTML report
hx bench -- --output bench-report.html
```

### Pass Framework-Specific Arguments

```bash
# Criterion: quick run
hx bench -- --quick

# Criterion: only one sample
hx bench -- --time-limit 1

# Gauge: JSON output
hx bench -- --json bench.json
```

## Benchmark Frameworks

### Criterion

The most popular Haskell benchmarking library:

```haskell
-- bench/Main.hs
import Criterion.Main

main :: IO ()
main = defaultMain
  [ bench "fib 20" $ whnf fib 20
  , bench "fib 30" $ whnf fib 30
  , bgroup "parsing"
    [ bench "small" $ whnf parse smallInput
    , bench "large" $ whnf parse largeInput
    ]
  ]
```

```bash
hx bench --release
```

### Gauge

A Criterion alternative:

```haskell
import Gauge.Main

main :: IO ()
main = defaultMain
  [ bench "function" $ whnf myFunction input
  ]
```

### Tasty-Bench

Integrates with Tasty:

```haskell
import Test.Tasty.Bench

main :: IO ()
main = defaultMain
  [ bench "reverse" $ whnf reverse [1..1000]
  , bcompare "reverse" $ bench "map id" $ whnf (map id) [1..1000]
  ]
```

## Configuration

Configure benchmarks in `hx.toml`:

```toml
[bench]
# Default benchmark suite
default-suite = "criterion-bench"

# Always use release mode
release = true

# Criterion-specific options
[bench.criterion]
template = "default"
output-dir = "bench-results"
```

## Best Practices

### Always Use Release Mode

Debug builds are not representative:

```bash
# Good
hx bench --release

# Bad - debug build
hx bench
```

### Minimize System Activity

Close other applications and disable background processes for consistent results.

### Run Multiple Times

```bash
# Run benchmarks multiple times
for i in {1..5}; do hx bench --release; done
```

### Compare Against Baseline

```bash
# Save baseline
hx bench --release -- --output baseline.csv

# Compare after changes
hx bench --release -- --output new.csv
```

## Cabal Configuration

Define benchmark suites in your `.cabal` file:

```cabal
benchmark my-benchmarks
  type: exitcode-stdio-1.0
  main-is: Main.hs
  hs-source-dirs: bench
  build-depends:
    , base
    , criterion
    , my-library
  ghc-options: -O2 -rtsopts

benchmark parsing-bench
  type: exitcode-stdio-1.0
  main-is: Parsing.hs
  hs-source-dirs: bench
  build-depends:
    , base
    , gauge
    , my-library
```

## RTS Options

Pass RTS options for memory profiling:

```bash
hx bench --release -- +RTS -s -RTS
```

## CI Integration

For CI, use non-interactive mode:

```bash
hx bench --release -- --quick --no-gc
```

## See Also

- [hx test](/docs/commands/test) — Run tests
- [hx build --release](/docs/commands/build) — Release builds
