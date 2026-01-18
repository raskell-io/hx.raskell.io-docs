+++
title = "Testing"
weight = 20
+++

# Test Suite

hx maintains high quality through comprehensive testing. This page documents our test strategy, how to run tests, and how to contribute.

---

## Test Suite Overview

| Category | Count | Framework | Location |
|----------|-------|-----------|----------|
| Unit tests | 430+ | Rust `#[test]` | `crates/*/src/` |
| Integration tests | 35+ | assert_cmd | `crates/*/tests/` |
| Solver benchmarks | 6 | Criterion | `crates/hx-solver/benches/` |
| CLI benchmarks | 7 | Criterion | `crates/hx-cli/benches/` |
| Snapshot tests | 15+ | insta | Various |

---

## Running Tests

### All Tests

```bash
# Run entire test suite
cargo test --workspace

# With output
cargo test --workspace -- --nocapture

# Single crate
cargo test -p hx-config
```

### Integration Tests Only

```bash
# All integration tests
cargo test --workspace --test '*'

# Specific test file
cargo test -p hx-cli --test e2e_workflows
```

### Benchmarks

```bash
# Solver benchmarks
cargo bench -p hx-solver

# CLI benchmarks
cargo bench -p hx-cli

# View HTML reports
open target/criterion/report/index.html
```

---

## Test Categories

### 1. Unit Tests

Unit tests verify individual functions and modules in isolation. They're located alongside the code in `#[cfg(test)]` modules.

**Well-Tested Areas:**
- Configuration parsing (`hx-config`)
- Version handling (`hx-core`)
- Dependency resolution (`hx-solver`)
- Cabal output parsing (`hx-cabal`)
- Toolchain detection (`hx-toolchain`)

**Example:**

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_version() {
        let v: Version = "1.2.3".parse().unwrap();
        assert_eq!(v.major, 1);
        assert_eq!(v.minor, 2);
        assert_eq!(v.patch, 3);
    }
}
```

---

### 2. Integration Tests

End-to-end tests verify complete workflows by invoking the actual `hx` binary.

**Location:** `crates/hx-cli/tests/e2e_workflows.rs`

**Tested Workflows:**
1. Project initialization (binary and library)
2. Configuration management
3. Build operations
4. Clean operations
5. Shell completions (bash, zsh, fish)
6. Help system navigation
7. Error handling
8. Verbose and quiet modes
9. Lockfile operations
10. Stackage integration
11. Cross-compilation flags
12. Server/LSP commands
13. Coverage commands

**Example:**

```rust
#[test]
fn test_workflow_create_build_simple() {
    let temp = TempDir::new().unwrap();
    let project_dir = temp.path().join("simple-app");

    // Initialize project
    hx().args(["init", "--name", "simple-app"])
        .arg(&project_dir)
        .assert()
        .success();

    // Verify structure
    assert!(project_dir.join("hx.toml").exists());
    assert!(project_dir.join("simple-app.cabal").exists());
    assert!(project_dir.join("src/Main.hs").exists());

    // Run doctor
    hx().current_dir(&project_dir)
        .arg("doctor")
        .assert()
        .success();
}
```

---

### 3. Snapshot Tests

Snapshot tests verify output stability using the `insta` crate. When output changes, you review and approve the diff.

**Use Cases:**
- Error message formatting
- Diagnostic output
- Generated files

**Example:**

```rust
#[test]
fn test_error_output_format() {
    let error = ConfigError::NotFound { path: "hx.toml".into() };
    insta::assert_snapshot!(format_error(&error));
}
```

**Managing Snapshots:**

```bash
# Review and update snapshots
cargo insta review

# Update all snapshots
cargo insta accept
```

---

### 4. Benchmarks

Performance benchmarks using the Criterion framework.

**Solver Benchmarks (`hx-solver`):**
- Simple resolution (10 packages, 3 versions)
- Scaling by package count (5, 10, 20, 50 packages)
- Scaling by version count (3, 5, 10, 20 versions)
- Multiple dependency resolution
- Index creation

**CLI Benchmarks (`hx-cli`):**
- Startup time (--help, --version)
- Project initialization (binary, library)
- Configuration parsing
- Clean operations
- Shell completions (bash, zsh, fish)
- Lockfile operations

See the [Benchmarks](/docs/development/benchmarks) page for detailed results.

---

## Test Infrastructure

### Frameworks

| Framework | Purpose |
|-----------|---------|
| `#[test]` | Built-in Rust unit testing |
| `#[tokio::test]` | Async test support |
| `assert_cmd` | CLI integration testing |
| `predicates` | Output assertions |
| `tempfile` | Temporary directory management |
| `insta` | Snapshot testing |
| `criterion` | Performance benchmarking |

### Test Fixtures

Fixtures are stored in `crates/<crate>/fixtures/`:

```
fixtures/
├── hx.toml/
│   ├── minimal.toml
│   ├── full.toml
│   └── invalid.toml
├── cabal-output/
│   ├── build-success.txt
│   └── build-failure.txt
└── lockfiles/
    ├── simple.lock
    └── complex.lock
```

### Mocking External Tools

For tests that need to mock GHC or other tools:

```rust
fn with_mock_ghc<F: FnOnce()>(version: &str, f: F) {
    let mock_dir = create_mock_ghc(version);
    let original_path = env::var("PATH").unwrap();
    env::set_var("PATH", format!("{}:{}", mock_dir.display(), original_path));
    f();
    env::set_var("PATH", original_path);
}
```

---

## CI/CD Pipeline

### Continuous Integration

Every commit and PR runs:

1. **Lint Job**
   - `cargo fmt --all -- --check`
   - `cargo clippy --workspace --all-targets -- -D warnings`

2. **Test Job** (3 platforms)
   - `cargo build --workspace`
   - `cargo test --workspace`
   - Platforms: Ubuntu, macOS, Windows

3. **Release Build**
   - `cargo build --release --workspace`

4. **Documentation**
   - `cargo doc --workspace --no-deps`
   - RUSTDOCFLAGS: `-D warnings`

### Platform Matrix

| Platform | Status |
|----------|--------|
| ubuntu-latest | Full support |
| macos-latest | Full support |
| windows-latest | Partial (some tests skipped) |

### Release Pipeline

Release builds target 6 platforms:
- `x86_64-unknown-linux-gnu`
- `x86_64-unknown-linux-musl`
- `aarch64-unknown-linux-gnu`
- `x86_64-apple-darwin`
- `aarch64-apple-darwin`
- `x86_64-pc-windows-msvc`

---

## Coverage Targets

| Crate | Target | Current |
|-------|--------|---------|
| hx-config | 80% | ~85% |
| hx-lock | 80% | ~82% |
| hx-solver | 75% | ~78% |
| hx-cabal | 70% | ~75% |
| hx-toolchain | 70% | ~72% |
| hx-cli | 60% | ~65% |

---

## Test Commands by Crate

### hx-config

```bash
cargo test -p hx-config

# Key tests:
# - TOML parsing
# - Default merging
# - Project root detection
# - Validation
```

### hx-solver

```bash
cargo test -p hx-solver
cargo bench -p hx-solver

# Key tests:
# - Version constraint parsing
# - Dependency resolution
# - Build plan generation
# - Hackage index loading
```

### hx-cli

```bash
cargo test -p hx-cli
cargo bench -p hx-cli

# Key tests:
# - Command argument parsing
# - Help output
# - Project initialization
# - End-to-end workflows
```

### hx-cabal

```bash
cargo test -p hx-cabal

# Key tests:
# - Cabal file parsing
# - GHC output parsing
# - Diagnostic extraction
# - Build state management
```

---

## Writing Tests

### Guidelines

1. **Test one thing per test**
   ```rust
   #[test]
   fn test_parse_simple_version() { ... }

   #[test]
   fn test_parse_version_with_prerelease() { ... }
   ```

2. **Use descriptive names**
   ```rust
   // Good
   #[test]
   fn test_parse_config_missing_toolchain_section() { ... }

   // Bad
   #[test]
   fn test1() { ... }
   ```

3. **Test error cases**
   ```rust
   #[test]
   fn test_parse_invalid_version_returns_error() {
       let result = "not.a.version".parse::<Version>();
       assert!(result.is_err());
   }
   ```

4. **Use fixtures for complex inputs**
   ```rust
   #[test]
   fn test_parse_complex_config() {
       let content = include_str!("../fixtures/hx.toml/full.toml");
       let config: Config = toml::from_str(content).unwrap();
       // assertions...
   }
   ```

### Adding New Tests

1. Add test in appropriate `#[cfg(test)]` module
2. Run `cargo test -p <crate>` to verify
3. Run `cargo fmt` and `cargo clippy`
4. Submit PR

---

## Debugging Failed Tests

### Verbose Output

```bash
# Show println! output
cargo test -- --nocapture

# Show test names as they run
cargo test -- --test-threads=1

# Run single test
cargo test test_parse_config
```

### Snapshot Updates

```bash
# Review and update snapshots
cargo insta review

# Update all snapshots
cargo insta accept
```

### Benchmark Comparison

```bash
# Compare against baseline
cargo bench -- --save-baseline master
git checkout feature-branch
cargo bench -- --baseline master
```

---

## Contributing Tests

We welcome test contributions:

1. **Add missing unit tests** for uncovered code paths
2. **Add integration tests** for new features
3. **Add regression tests** for fixed bugs
4. **Improve test documentation**

### Test Contribution Checklist

- [ ] Test passes locally
- [ ] Test has descriptive name
- [ ] Test verifies one specific behavior
- [ ] Test includes error cases where appropriate
- [ ] Test doesn't depend on external state
- [ ] Test cleans up after itself

Submit test improvements: [GitHub PRs](https://github.com/raskell-io/hx/pulls)

---

## Related Pages

- [Benchmarks](/docs/development/benchmarks) — Performance measurements
- [Contributing](https://github.com/raskell-io/hx/blob/main/CONTRIBUTING.md) — How to contribute
