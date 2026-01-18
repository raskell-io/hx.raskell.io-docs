+++
title = "Watch Mode"
weight = 5
+++

hx's watch mode automatically runs commands when source files change, enabling rapid development iteration.

## Overview

Watch mode monitors your source files and triggers rebuilds, tests, or other commands automatically when you save changes.

```bash
hx watch build
```

## Basic Usage

### Watch and Build

```bash
hx watch build
```

Rebuilds automatically when any `.hs` file changes.

### Watch and Test

```bash
hx watch test
```

Re-runs tests on every save.

### Watch and Run

```bash
hx watch run
```

Rebuilds and restarts the application.

### Watch and Check

```bash
hx watch check
```

Type-checks without full compilation (faster feedback).

## Watched Files

By default, hx watches:

| Pattern | Description |
|---------|-------------|
| `*.hs` | Haskell source files |
| `*.cabal` | Cabal configuration |
| `hx.toml` | hx configuration |
| `*.yaml`, `*.json` | Common config files |

In directories:
- `src/`
- `app/`
- `lib/`
- `test/`
- `bench/`

## Options

### Clear Terminal

Clear the screen before each run:

```bash
hx watch build --clear
```

### Debounce

Control how long to wait after changes settle:

```bash
# Wait 500ms after last change
hx watch build --debounce 500
```

Default is 300ms.

### Custom Patterns

Watch additional files:

```bash
hx watch build --include "config/**/*.yaml"
```

Exclude files:

```bash
hx watch build --exclude "generated/**"
```

### Polling Mode

Use polling instead of filesystem events (useful for network drives):

```bash
hx watch build --poll
```

## Configuration

Configure watch behavior in `hx.toml`:

```toml
[watch]
# Clear terminal on each run
clear = true

# Debounce delay (ms)
debounce = 300

# Additional patterns to watch
include = ["config/**"]

# Patterns to exclude
exclude = [
  "dist-newstyle/**",
  ".git/**",
  "*.tmp"
]
```

## Workflows

### Test-Driven Development

```bash
hx watch test
```

1. Write a failing test
2. Save → tests run automatically
3. Write implementation
4. Save → tests run again
5. See green tests
6. Repeat

### REPL-Like Development

```bash
hx watch run
```

1. Make changes to your application
2. Save → app rebuilds and restarts
3. See results immediately

### Type-First Development

```bash
hx watch check
```

Faster feedback than full builds:

1. Write function signatures
2. Save → type errors shown
3. Fill in implementations
4. Save → type check again

### Combined Workflows

Run checks, then tests if they pass:

```bash
# Using shell
watch -n 1 'hx check && hx test --match "fast"'
```

## Keyboard Controls

While in watch mode:

| Key | Action |
|-----|--------|
| `r` | Force re-run |
| `Enter` | Force re-run |
| `q` | Quit watch mode |
| `Ctrl+C` | Quit watch mode |

## Performance Tips

### Use Check for Fast Feedback

`hx watch check` is faster than `hx watch build`:

```bash
# Faster - type check only
hx watch check

# Slower - full compilation
hx watch build
```

### Exclude Generated Files

```toml
[watch]
exclude = ["generated/**", "dist-newstyle/**"]
```

### Adjust Debounce

Lower debounce for faster response:

```toml
[watch]
debounce = 100
```

Higher debounce to batch rapid changes:

```toml
[watch]
debounce = 500
```

### Focus on Changed Areas

Run specific tests:

```bash
hx watch test -- --match "Parser"
```

## Integration with Editors

### VS Code

HLS provides real-time feedback. Use watch mode for tests:

1. Open integrated terminal
2. Run `hx watch test`
3. Keep terminal visible while editing

### Vim/Neovim

Split terminal workflow:

```vim
:split | terminal hx watch test
```

Or with tmux:
```bash
tmux split-window -h "hx watch test"
```

### Emacs

```elisp
(defun hx-watch-test ()
  (interactive)
  (async-shell-command "hx watch test"))
```

## Troubleshooting

### Changes Not Detected

Try polling mode:

```bash
hx watch build --poll
```

Check watched directories:

```bash
hx watch build --verbose
```

### Too Many Rebuilds

Increase debounce:

```bash
hx watch build --debounce 500
```

Exclude noisy directories:

```toml
[watch]
exclude = ["logs/**", "tmp/**"]
```

### High CPU Usage

On some systems, filesystem watching can be intensive. Solutions:

1. Use polling with longer intervals:
   ```bash
   hx watch build --poll
   ```

2. Reduce watched directories

## Platform Notes

### Linux

Uses inotify. May need to increase watch limit:

```bash
echo fs.inotify.max_user_watches=524288 | sudo tee -a /etc/sysctl.conf
sudo sysctl -p
```

### macOS

Uses FSEvents. Generally works well.

### Windows

Uses ReadDirectoryChangesW. May have issues with network drives—use `--poll` if needed.

## See Also

- [hx build](/docs/commands/build) — Build command
- [hx test](/docs/commands/test) — Test command
- [hx check](/docs/commands/check) — Type check command
