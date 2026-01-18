+++
title = "Troubleshooting"
weight = 20
+++

Solutions to common problems with hx.

## Quick Diagnostics

Start with the doctor:

```bash
hx doctor
```

This checks:
- Toolchain installation
- Version compatibility
- Project configuration
- System dependencies

## Installation Issues

### hx Command Not Found

After installation:

```
bash: hx: command not found
```

**Solution**: Add to PATH

```bash
# Add to ~/.bashrc or ~/.zshrc
export PATH="$HOME/.local/bin:$PATH"

# Reload shell
source ~/.bashrc
```

### Permission Denied

```
error: Permission denied installing to /usr/local/bin
```

**Solution**: Use user-local install

```bash
curl -fsSL https://get.raskell.io/hx | sh -s -- --prefix ~/.local
```

## Toolchain Issues

### GHC Not Found

```
error: GHC not found in PATH
```

**Solution**: Install GHC

```bash
hx toolchain install --ghc 9.8.2
```

### GHC Version Mismatch

```
error: GHC version mismatch
  required: 9.8.2 (from hx.toml)
  found: 9.6.4
```

**Solution**: Install and use correct version

```bash
hx toolchain install --ghc 9.8.2
hx toolchain use --ghc 9.8.2
```

### HLS Incompatibility

```
warning: HLS 2.6.0 may not support GHC 9.10.1
```

**Solution**: Check HLS compatibility or use supported GHC

```bash
# Use compatible GHC version
hx toolchain use --ghc 9.8.2
```

## Build Issues

### Package Not Found

```
error: Could not resolve dependencies
  unknown package: some-package
```

**Solution**: Update package index

```bash
cabal update
hx lock
```

### Dependency Conflict

```
error: Dependency conflict
  package-a requires text <2
  package-b requires text >=2
```

**Solutions**:

1. Update conflicting package:
   ```bash
   hx update package-a
   ```

2. Check for newer versions:
   ```bash
   hx outdated
   ```

3. Use allow-newer (escape hatch):
   ```cabal
   -- cabal.project
   allow-newer: package-a:text
   ```

### Build Fails After GHC Upgrade

```
error: Module 'Foo' is not loaded
```

**Solution**: Clean and rebuild

```bash
hx clean
hx build
```

### Out of Memory

```
error: ghc: out of memory
```

**Solutions**:

1. Reduce parallelism:
   ```bash
   hx build -j1
   ```

2. Increase heap size:
   ```bash
   hx build --ghc-options="+RTS -M8G -RTS"
   ```

3. Enable split sections:
   ```toml
   [build]
   split-sections = true
   ```

## Lockfile Issues

### Lockfile Out of Sync

```
error: hx.lock is out of sync with hx.toml
```

**Solution**: Regenerate lockfile

```bash
hx lock
```

### Lockfile Merge Conflict

After git merge:

```
CONFLICT (content): Merge conflict in hx.lock
```

**Solution**: Regenerate

```bash
# Accept one version
git checkout --ours hx.lock

# Regenerate
hx lock

# Verify
hx build
```

### Checksum Mismatch

```
error: Package checksum mismatch
  expected: abc123...
  got: def456...
```

**Solution**: Force regenerate

```bash
hx lock --force
hx sync
```

## Configuration Issues

### Invalid hx.toml

```
error: Failed to parse hx.toml
  line 5: expected string, found integer
```

**Solution**: Check TOML syntax

```toml
# Strings need quotes
[project]
name = "my-project"  # Correct
name = my-project    # Wrong
```

### Unknown Configuration Key

```
warning: Unknown key 'build.optimize' in hx.toml
```

**Solution**: Check documentation for correct keys

```toml
# Correct key
[build]
release = true

# Not: optimize = true
```

## Test Issues

### Tests Not Found

```
No tests to run
```

**Solution**: Check test configuration

```cabal
test-suite my-tests
  type: exitcode-stdio-1.0
  main-is: Spec.hs
  hs-source-dirs: test
```

Verify test file exists and has correct module:

```haskell
-- test/Spec.hs
module Main where

import Test.Hspec

main :: IO ()
main = hspec spec
```

### Test Timeout

```
error: Test timed out after 60s
```

**Solution**: Increase timeout

```bash
hx test -- --timeout 120s
```

## Watch Mode Issues

### Changes Not Detected

```
Watching for changes...
# But saves don't trigger rebuild
```

**Solutions**:

1. Use polling mode:
   ```bash
   hx watch build --poll
   ```

2. Check watch limits (Linux):
   ```bash
   echo fs.inotify.max_user_watches=524288 | sudo tee -a /etc/sysctl.conf
   sudo sysctl -p
   ```

### Too Many Rebuilds

**Solution**: Increase debounce

```bash
hx watch build --debounce 500
```

## Platform-Specific Issues

### Linux: Missing Libraries

```
error: libgmp.so: cannot open shared object file
```

**Solution**: Install system dependencies

```bash
# Ubuntu/Debian
sudo apt install libgmp-dev libncurses-dev

# Fedora
sudo dnf install gmp-devel ncurses-devel
```

### macOS: Library Not Loaded

```
dyld: Library not loaded: /usr/local/opt/gmp/lib/libgmp.10.dylib
```

**Solution**: Install via Homebrew

```bash
brew install gmp
```

### Windows: Long Path Issues

```
error: The specified path, file name, or both are too long
```

**Solution**: Enable long paths

```powershell
# Run as Administrator
New-ItemProperty -Path "HKLM:\SYSTEM\CurrentControlSet\Control\FileSystem" `
  -Name "LongPathsEnabled" -Value 1 -PropertyType DWORD -Force
```

## Getting Help

### Verbose Output

```bash
hx build --verbose
```

Shows full command and output.

### Debug Mode

```bash
HX_DEBUG=1 hx build
```

Shows internal debugging info.

### Reporting Issues

Include in bug reports:
1. `hx --version`
2. `hx doctor` output
3. Relevant configuration files
4. Full error message with `--verbose`

File issues at: https://github.com/raskell-io/hx/issues

## See Also

- [hx doctor](/docs/commands/doctor) — Diagnostics
- [Installation](/docs/installation) — Setup guide
- [Configuration](/docs/configuration/hx-toml) — Config reference
