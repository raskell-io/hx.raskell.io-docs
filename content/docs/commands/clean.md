+++
title = "hx clean"
weight = 40
+++

Remove build artifacts.

## Synopsis

```bash
hx clean [OPTIONS]
```

## Description

The `clean` command removes build artifacts from your project, freeing disk space and ensuring a fresh build.

## Options

```
    --full              Remove all artifacts including cached dependencies
    --dist              Remove only dist-newstyle (default)
    --hx                Remove .hx directory
    --dry-run           Show what would be removed
-v, --verbose           Show detailed output
```

## Examples

### Standard Clean

```bash
hx clean
```

Removes:
- `dist-newstyle/` directory

### Full Clean

```bash
hx clean --full
```

Removes:
- `dist-newstyle/`
- `.hx/` (project metadata)
- Local package cache

### Dry Run

```bash
hx clean --dry-run
```

Output:
```
Would remove:
  dist-newstyle/  (1.2 GB)
```

### Clean .hx Directory

```bash
hx clean --hx
```

Removes hx-specific project metadata.

## What Gets Removed

### Default (`--dist`)

```
dist-newstyle/
├── build/           # Compiled objects
├── cache/           # Build cache
├── packagedb/       # Package database
└── tmp/             # Temporary files
```

### Full (`--full`)

```
dist-newstyle/
.hx/
  ├── cache/         # hx-specific cache
  └── bhc.toml       # Generated BHC manifest
```

## When to Clean

Clean when:
- Disk space is low
- Build is corrupted
- Switching GHC versions
- Debugging build issues

Usually NOT needed:
- Regular development (incremental builds are faster)
- Before committing (build artifacts are gitignored)

## Disk Usage

Check build directory size:

```bash
du -sh dist-newstyle
```

Sample output:
```
2.1G    dist-newstyle
```

## Rebuild After Clean

After cleaning, rebuild:

```bash
hx clean
hx build
```

Note: First build after clean takes longer as everything recompiles.

## .gitignore

These directories should be in `.gitignore`:

```gitignore
dist-newstyle/
.hx/
```

hx includes these in new projects automatically.

## See Also

- [hx build](/docs/commands/build) — Build the project
