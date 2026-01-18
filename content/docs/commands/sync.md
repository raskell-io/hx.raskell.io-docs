+++
title = "hx sync"
weight = 21
+++

Synchronize dependencies from the lockfile.

## Synopsis

```bash
hx sync [OPTIONS]
```

## Description

The `sync` command installs dependencies according to `hx.lock`, ensuring your local environment matches the locked versions exactly.

## Options

```
    --force             Reinstall all dependencies
    --offline           Use only cached packages
    --check             Verify sync without installing
-v, --verbose           Show detailed output
```

## Examples

### Sync Dependencies

```bash
hx sync
```

### Force Reinstall

```bash
hx sync --force
```

### Offline Mode

```bash
# Use only locally cached packages
hx sync --offline
```

### Check Without Installing

```bash
hx sync --check
```

## When to Sync

Run `hx sync` when:

- Cloning a project with `hx.lock`
- Checking out a different branch
- After pulling changes to `hx.lock`
- After modifying `hx.lock` manually

## Typical Workflow

### New Clone

```bash
git clone https://github.com/example/project
cd project
hx sync
hx build
```

### Branch Switch

```bash
git checkout feature-branch
hx sync  # Install branch's dependencies
hx build
```

### After Pull

```bash
git pull
hx sync  # Install updated dependencies
hx build
```

## What It Does

1. **Reads hx.lock** for exact versions
2. **Downloads packages** from Hackage (if needed)
3. **Verifies checksums** against lockfile
4. **Installs packages** to local store
5. **Configures build** to use locked versions

## Lockfile Required

`hx sync` requires `hx.lock` to exist:

```
error: hx.lock not found

fix: Run `hx lock` to generate a lockfile
```

## Offline Mode

For air-gapped environments:

```bash
# First, on a connected machine:
hx sync  # Downloads all packages

# Later, on disconnected machine:
hx sync --offline
```

Packages are cached in:
- Linux: `~/.cache/hx/packages`
- macOS: `~/Library/Caches/hx/packages`
- Windows: `%LOCALAPPDATA%\hx\packages`

## CI Usage

```yaml
- name: Install dependencies
  run: hx sync

- name: Build
  run: hx build

- name: Test
  run: hx test
```

## See Also

- [hx lock](/docs/commands/lock) — Generate lockfile
- [hx update](/docs/commands/update) — Update dependencies
