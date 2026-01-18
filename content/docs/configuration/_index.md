+++
title = "Configuration Reference"
weight = 20
template = "section.html"
sort_by = "weight"
+++

hx is configured through `hx.toml`, a TOML configuration file in your project root.

## Configuration Files

| File | Purpose |
|------|---------|
| `hx.toml` | Project configuration |
| `hx.lock` | Lockfile (auto-generated) |
| `.hx/` | Project-local cache/metadata |

## Quick Reference

```toml
[project]
name = "my-project"
version = "0.1.0"

[toolchain]
ghc = "9.8.2"
cabal = "3.10.3.0"

[compiler]
backend = "ghc"

[build]
release = false
jobs = 4
ghc-options = ["-Wall"]

[test]
fail-fast = false

[fmt]
formatter = "ormolu"

[lint]
ignore = []
```

## Sections

- **[hx.toml Reference](/docs/configuration/hx-toml)** — Complete configuration file reference
- **[Lockfile](/docs/configuration/lockfile)** — Understanding hx.lock
- **[Environment Variables](/docs/configuration/environment)** — Environment variable configuration
