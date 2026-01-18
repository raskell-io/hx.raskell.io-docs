+++
title = "hx fmt"
weight = 41
+++

Format Haskell source code.

## Synopsis

```bash
hx fmt [OPTIONS] [FILES]...
```

## Description

The `fmt` command formats Haskell source files using a code formatter. By default, it uses Ormolu, but can be configured to use Fourmolu or other formatters.

## Arguments

```
FILES...                Files to format (default: all .hs files)
```

## Options

```
    --check             Check formatting without modifying files
    --diff              Show diff of changes
    --formatter <NAME>  Formatter to use [ormolu, fourmolu, stylish]
    --config <FILE>     Custom formatter config file
-v, --verbose           Show detailed output
```

## Examples

### Format All Files

```bash
hx fmt
```

### Format Specific Files

```bash
hx fmt src/MyModule.hs app/Main.hs
```

### Check Formatting (CI)

```bash
hx fmt --check
```

Exit code 0 if formatted, 1 if changes needed.

### Show Diff

```bash
hx fmt --diff
```

### Use Fourmolu

```bash
hx fmt --formatter fourmolu
```

## Formatters

### Ormolu (Default)

Opinionated, minimal configuration:
- Consistent output
- No configuration options
- Widely adopted

### Fourmolu

Ormolu fork with more options:
- Configurable via `fourmolu.yaml`
- More formatting options
- Compatible with Ormolu style

### Stylish Haskell

Traditional formatter:
- Highly configurable
- `.stylish-haskell.yaml` config
- Modular formatting

## Configuration

Configure in `hx.toml`:

```toml
[fmt]
# Formatter to use
formatter = "ormolu"

# Check only (don't modify)
check = false

# Exclude patterns
exclude = ["generated/**"]
```

### Fourmolu Configuration

Create `fourmolu.yaml`:

```yaml
indentation: 2
function-arrows: leading
comma-style: leading
import-export-style: leading
indent-wheres: true
record-brace-space: true
newlines-between-decls: 1
```

### Stylish Haskell Configuration

Create `.stylish-haskell.yaml`:

```yaml
steps:
  - imports:
      align: global
  - language_pragmas:
      style: vertical
  - trailing_whitespace: {}
```

## Editor Integration

### VS Code

Install "Haskell" extension, configure:

```json
{
  "haskell.formattingProvider": "ormolu"
}
```

### Vim/Neovim

With ALE:

```vim
let g:ale_fixers = {'haskell': ['ormolu']}
let g:ale_fix_on_save = 1
```

### Emacs

With haskell-mode:

```elisp
(setq haskell-mode-stylish-haskell-path "ormolu")
```

## Pre-commit Hook

Format on commit:

```bash
# .git/hooks/pre-commit
#!/bin/sh
hx fmt --check || {
    echo "Run 'hx fmt' to format code"
    exit 1
}
```

Or use hx with git hooks:

```bash
# Create hook
echo '#!/bin/sh\nhx fmt --check' > .git/hooks/pre-commit
chmod +x .git/hooks/pre-commit
```

## CI Integration

```yaml
- name: Check formatting
  run: hx fmt --check
```

## See Also

- [hx lint](/docs/commands/lint) — Run linter
- [Configuration Reference](/docs/configuration/hx-toml)
