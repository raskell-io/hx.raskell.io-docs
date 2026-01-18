+++
title = "hx doctor"
weight = 50
+++

Diagnose environment and configuration issues.

## Synopsis

```bash
hx doctor [OPTIONS]
```

## Description

The `doctor` command checks your development environment for common issues and provides actionable fixes. It verifies toolchain installations, configuration files, and system dependencies.

## Options

```
    --fix               Attempt to automatically fix issues
    --json              Output as JSON
    --check <COMPONENT> Check specific component only
-v, --verbose           Show detailed output
```

## Examples

### Run Full Diagnostics

```bash
hx doctor
```

### JSON Output

```bash
hx doctor --json
```

### Auto-Fix Issues

```bash
hx doctor --fix
```

### Check Specific Component

```bash
hx doctor --check ghc
hx doctor --check cabal
hx doctor --check bhc
```

## Sample Output

```
hx doctor

Checking environment...

✓ GHC 9.8.2 installed at /Users/user/.ghcup/bin/ghc
✓ Cabal 3.10.3.0 installed at /Users/user/.ghcup/bin/cabal
✓ GHCup 0.1.22.0 installed at /Users/user/.ghcup/bin/ghcup
✓ HLS 2.6.0.0 installed at /Users/user/.ghcup/bin/haskell-language-server-wrapper
✗ BHC not installed

Checking project...

✓ hx.toml found
✓ Cabal file found: my-project.cabal
✓ hx.lock found and up-to-date
⚠ GHC version mismatch
    hx.toml requires: 9.8.2
    Active GHC: 9.6.4

Summary: 1 error, 1 warning

Fixes available:
  • Run `hx toolchain install --ghc 9.8.2` to install required GHC
  • Run `hx toolchain use --ghc 9.8.2` to set active version
```

## Checks Performed

### Toolchain Checks

| Check | Description |
|-------|-------------|
| GHC installed | GHC compiler is available |
| GHC version | Matches hx.toml requirement |
| Cabal installed | Cabal build tool is available |
| GHCup installed | Toolchain manager (optional) |
| HLS installed | Language server (optional) |
| BHC installed | Alternative compiler (if configured) |

### Project Checks

| Check | Description |
|-------|-------------|
| hx.toml exists | Configuration file present |
| hx.toml valid | Configuration is parseable |
| Cabal file exists | Package description present |
| Lockfile exists | hx.lock is present |
| Lockfile in sync | Matches hx.toml |

### System Checks

| Check | Description |
|-------|-------------|
| PATH configured | Tools are in PATH |
| Disk space | Sufficient for builds |
| Permissions | Write access to project |

## Severity Levels

### ✓ Pass

Everything is working correctly.

### ⚠ Warning

Something is suboptimal but won't prevent builds:
- Outdated tool versions
- Missing optional tools (HLS)
- Non-critical configuration issues

### ✗ Error

Will prevent builds:
- Missing required tools
- Version mismatches
- Invalid configuration

## Auto-Fix

Use `--fix` to automatically resolve issues:

```bash
hx doctor --fix
```

Can fix:
- Missing toolchain versions (installs them)
- Version mismatches (sets active version)
- Missing hx.lock (generates it)

Cannot fix:
- Invalid configuration (requires manual edit)
- Disk space issues
- Permission problems

## CI Usage

Use in CI to verify environment:

```yaml
- name: Check environment
  run: hx doctor

# Or fail on any issue
- name: Check environment (strict)
  run: |
    hx doctor --json | jq -e '.errors == 0'
```

## Common Issues

### GHC Not Found

```
✗ GHC not found in PATH

fix: Install GHC with:
     hx toolchain install --ghc 9.8.2
```

### Version Mismatch

```
⚠ GHC version mismatch
    Required: 9.8.2
    Found: 9.6.4

fix: Run `hx toolchain use --ghc 9.8.2`
     or `hx toolchain install --ghc 9.8.2`
```

### HLS Mismatch

```
⚠ HLS may not support GHC 9.10.1
    HLS version: 2.6.0.0
    Supported GHC: 9.8.x, 9.6.x

hint: Check HLS compatibility at
      https://haskell-language-server.readthedocs.io
```

### Lockfile Out of Date

```
✗ hx.lock is out of sync

fix: Run `hx lock` to update
```

### BHC Not Installed

```
✗ BHC not installed (required by hx.toml)
    [compiler]
    backend = "bhc"

fix: Run `hx toolchain install --bhc 0.2.0`
```

## See Also

- [hx toolchain](/docs/commands/toolchain) — Manage toolchains
- [Installation](/docs/installation) — Setup guide
- [Troubleshooting](/docs/guides/troubleshooting) — Common issues
