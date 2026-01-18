+++
title = "Architecture"
weight = 4
+++

How hx is built internally.

## Overview

hx is a Rust CLI application that orchestrates existing Haskell tools. It follows a modular crate architecture with clear separation of concerns.

## Design Philosophy

### Wrap Then Replace

hx wraps existing tools rather than replacing them:

1. **Wrap** — Call existing tools (Cabal, GHC)
2. **Tame** — Provide better UX on top
3. **Replace** — Optionally replace components later

This approach provides:
- Immediate compatibility
- Gradual improvement
- Safe fallback

### Orchestration Layer

hx acts as an orchestrator:

```
hx build
    │
    ▼
Read config (hx.toml)
    │
    ▼
Detect toolchain
    │
    ▼
Call cabal build (or bhc build)
    │
    ▼
Parse output
    │
    ▼
Display formatted results
```

## Crate Structure

hx is organized as a Rust workspace:

```
hx/
├── Cargo.toml             # Workspace definition
├── crates/
│   ├── hx-cli/            # CLI entry point
│   ├── hx-core/           # Core orchestration
│   ├── hx-config/         # Configuration parsing
│   ├── hx-toolchain/      # Toolchain management
│   ├── hx-compiler/       # Compiler abstraction
│   ├── hx-cabal/          # Cabal wrapper
│   ├── hx-bhc/            # BHC backend
│   ├── hx-lock/           # Lockfile handling
│   ├── hx-doctor/         # Diagnostics
│   ├── hx-ui/             # Terminal UI
│   └── hx-cache/          # Caching
```

### Crate Responsibilities

| Crate | Responsibility |
|-------|----------------|
| `hx-cli` | Command parsing, dispatch |
| `hx-core` | Project context, orchestration |
| `hx-config` | Parse hx.toml, merge defaults |
| `hx-toolchain` | Detect/install GHC, Cabal, BHC |
| `hx-compiler` | Compiler backend abstraction |
| `hx-cabal` | Cabal command execution |
| `hx-bhc` | BHC backend implementation |
| `hx-lock` | Lockfile read/write |
| `hx-doctor` | Health checks |
| `hx-ui` | Progress, spinners, colors |
| `hx-cache` | Artifact caching |

## Key Abstractions

### CompilerBackend Trait

Unified interface for different compilers:

```rust
#[async_trait]
pub trait CompilerBackend: Send + Sync {
    fn name(&self) -> &str;
    fn description(&self) -> &str;

    async fn detect(&self) -> Result<CompilerStatus>;
    async fn version(&self) -> Result<String>;

    async fn build(
        &self,
        project_root: &Path,
        options: &BuildOptions,
        output: &Output,
    ) -> Result<BuildResult>;

    async fn check(
        &self,
        project_root: &Path,
        options: &CheckOptions,
        output: &Output,
    ) -> Result<CheckResult>;

    async fn run(
        &self,
        project_root: &Path,
        options: &RunOptions,
        output: &Output,
    ) -> Result<RunResult>;

    fn parse_diagnostics(&self, raw_output: &str) -> Vec<Diagnostic>;
}
```

Implementations:
- `GhcBackend` — Wraps Cabal for GHC builds
- `BhcBackend` — Invokes BHC directly

### Configuration Layering

Settings are merged from multiple sources:

```rust
struct Config {
    // Merged from:
    // 1. Built-in defaults
    // 2. User config (~/.config/hx/config.toml)
    // 3. Project config (hx.toml)
    // 4. Environment variables
    // 5. Command-line flags
}
```

### Diagnostic System

Structured error messages:

```rust
pub struct Diagnostic {
    pub severity: Severity,      // Error, Warning, Info
    pub code: Option<String>,    // E001, W042
    pub message: String,         // What went wrong
    pub location: Option<Location>,
    pub fixes: Vec<Fix>,         // Suggested fixes
    pub hints: Vec<String>,      // Additional context
}
```

## Command Flow

Example: `hx build --release`

```
hx-cli
  │
  ├─ Parse args with clap
  │
  └─ Call build::execute()
         │
         ├─ hx-config: Load hx.toml
         │
         ├─ hx-toolchain: Verify GHC available
         │
         ├─ hx-lock: Check lockfile
         │
         ├─ hx-compiler: Get backend
         │     │
         │     └─ hx-cabal (or hx-bhc)
         │           │
         │           └─ Execute: cabal build --ghc-options=-O2
         │
         ├─ Parse stdout/stderr
         │
         └─ hx-ui: Display results
```

## Error Handling

### Error Types

Each crate defines its own errors:

```rust
// hx-config
#[derive(Debug, thiserror::Error)]
pub enum ConfigError {
    #[error("failed to read config: {path}")]
    ReadError { path: PathBuf, source: io::Error },

    #[error("invalid TOML: {0}")]
    ParseError(#[from] toml::de::Error),
}
```

### Error Propagation

Errors bubble up with context:

```rust
fn load_config(path: &Path) -> Result<Config> {
    let contents = fs::read_to_string(path)
        .map_err(|e| ConfigError::ReadError {
            path: path.to_path_buf(),
            source: e,
        })?;

    toml::from_str(&contents)?
}
```

### User-Facing Errors

CLI converts to user-friendly format:

```
error: Failed to read config
  path: /project/hx.toml
  cause: Permission denied

fix: Check file permissions
     chmod 644 hx.toml
```

## Async Runtime

hx uses Tokio for async operations:

```rust
#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();
    match cli.command {
        Commands::Build(args) => build::execute(args).await,
        Commands::Test(args) => test::execute(args).await,
        // ...
    }
}
```

Async is used for:
- Process execution
- File I/O
- Network requests (downloads)
- Concurrent operations

## Testing Strategy

### Unit Tests

In each crate's `src/`:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_config() {
        let toml = r#"
            [project]
            name = "test"
        "#;
        let config: Config = toml::from_str(toml).unwrap();
        assert_eq!(config.project.name, "test");
    }
}
```

### Integration Tests

In `tests/` directories:

```rust
#[test]
fn test_init_creates_files() {
    let temp = tempdir().unwrap();
    // Run hx init
    // Verify files created
}
```

### Fixture-Based Tests

Using golden files:

```rust
#[test]
fn test_error_format() {
    let error = make_error();
    let output = format_error(&error);
    insta::assert_snapshot!(output);
}
```

## Extension Points

### Adding a New Command

1. Add to `hx-cli/src/commands/`
2. Register in `hx-cli/src/cli.rs`
3. Implement command logic

### Adding a New Compiler Backend

1. Create new crate (`hx-newbackend`)
2. Implement `CompilerBackend` trait
3. Register in `hx-compiler/src/registry.rs`

### Adding Configuration Options

1. Add to `hx-config/src/manifest.rs`
2. Add `Combine` impl in `hx-config/src/combine.rs`
3. Document in user docs

## Dependencies

Key Rust dependencies:

| Crate | Purpose |
|-------|---------|
| `clap` | CLI parsing |
| `tokio` | Async runtime |
| `serde` | Serialization |
| `toml` | TOML parsing |
| `tracing` | Logging |
| `thiserror` | Error definitions |
| `anyhow` | Error handling |
| `indicatif` | Progress bars |
| `console` | Terminal colors |

## See Also

- [GitHub Repository](https://github.com/raskell-io/hx)
- [Contributing Guide](https://github.com/raskell-io/hx/blob/main/CONTRIBUTING.md)
