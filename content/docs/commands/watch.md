+++
title = "hx watch"
weight = 43
+++

Watch for file changes and run commands automatically.

## Synopsis

```bash
hx watch <COMMAND> [OPTIONS] [-- <ARGS>...]
```

## Description

The `watch` command monitors your source files for changes and automatically runs a specified command when files are modified. This enables a rapid development feedback loop.

## Arguments

```
COMMAND                 Command to run: build, test, run, check
```

## Options

```
    --debounce <MS>     Debounce delay in milliseconds (default: 300)
    --clear             Clear terminal before each run
    --poll              Use polling instead of filesystem events
    --include <GLOB>    Additional file patterns to watch
    --exclude <GLOB>    Patterns to exclude
-v, --verbose           Show detailed output
```

## Examples

### Watch and Build

```bash
hx watch build
```

### Watch and Test

```bash
hx watch test
```

### Watch and Run

```bash
hx watch run
```

With arguments:
```bash
hx watch run -- --port 8080
```

### Watch and Check

```bash
hx watch check
```

### Clear Terminal on Rebuild

```bash
hx watch build --clear
```

### Custom Debounce

```bash
# Wait 500ms after last change before running
hx watch build --debounce 500
```

### Watch Additional Files

```bash
# Also watch config files
hx watch build --include "config/**/*.yaml"
```

### Exclude Patterns

```bash
# Ignore generated files
hx watch build --exclude "generated/**"
```

## Watched Files

By default, hx watches:

- `*.hs` - Haskell source files
- `*.cabal` - Cabal configuration
- `hx.toml` - hx configuration
- `*.yaml`, `*.json` - Common config files

In these directories:
- `src/`
- `app/`
- `lib/`
- `test/`
- `bench/`

## Debouncing

When multiple files change quickly (e.g., saving all buffers), hx waits for changes to settle before running:

```
File changed: src/Foo.hs
File changed: src/Bar.hs
[waiting 300ms...]
Running build...
```

Adjust with `--debounce`:

```bash
# Faster response (might trigger multiple builds)
hx watch build --debounce 100

# More conservative (waits longer)
hx watch build --debounce 1000
```

## Terminal Behavior

### Without --clear

Output accumulates:
```
[12:00:01] Building...
[12:00:03] Build succeeded
[12:00:15] Building...
[12:00:17] Build succeeded
```

### With --clear

Terminal is cleared between runs:
```
[12:00:17] Building...
Build succeeded
```

## Keyboard Shortcuts

While watching:

| Key | Action |
|-----|--------|
| `r` | Force re-run |
| `q` | Quit watch mode |
| `Enter` | Force re-run |

## Configuration

Configure watch behavior in `hx.toml`:

```toml
[watch]
# Clear terminal on each run
clear = true

# Debounce delay
debounce = 300

# Additional patterns to watch
include = ["config/**"]

# Patterns to exclude
exclude = ["dist-newstyle/**", ".git/**"]
```

## Advanced Workflows

### TDD Workflow

```bash
# Run tests on every save
hx watch test
```

### REPL-like Development

```bash
# Run the program on every save
hx watch run
```

### Continuous Type Checking

```bash
# Fast feedback without full builds
hx watch check
```

### Running Multiple Commands

Use a shell command:

```bash
watch -n 1 'hx check && hx test --match "fast"'
```

## Platform Notes

### Linux

Uses inotify for efficient file watching.

### macOS

Uses FSEvents for efficient file watching.

### Windows

Uses ReadDirectoryChangesW.

### Polling Mode

If filesystem events are unreliable (network drives, containers):

```bash
hx watch build --poll
```

## See Also

- [hx build](/docs/commands/build) — Build command
- [hx test](/docs/commands/test) — Test command
- [hx check](/docs/commands/check) — Type check command
