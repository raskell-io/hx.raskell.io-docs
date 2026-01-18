+++
title = "Formatting & Linting"
weight = 6
+++

hx integrates code formatting and linting tools for maintaining code quality.

## Overview

hx provides unified commands for:
- **Formatting**: Consistent code style with Ormolu, Fourmolu, or Stylish Haskell
- **Linting**: Code improvements with HLint

## Formatting

### Basic Usage

Format all files:

```bash
hx fmt
```

Format specific files:

```bash
hx fmt src/MyModule.hs app/Main.hs
```

Check formatting without modifying:

```bash
hx fmt --check
```

### Formatters

hx supports multiple formatters:

| Formatter | Description | Configuration |
|-----------|-------------|---------------|
| Ormolu | Opinionated, no config | None |
| Fourmolu | Ormolu fork, configurable | `fourmolu.yaml` |
| Stylish Haskell | Traditional, highly configurable | `.stylish-haskell.yaml` |

### Ormolu (Default)

Zero configuration formatter:

```bash
hx fmt
```

Ormolu enforces a single style with no options.

### Fourmolu

Configurable Ormolu fork:

```bash
hx fmt --formatter fourmolu
```

Create `fourmolu.yaml`:

```yaml
indentation: 2
function-arrows: leading
comma-style: leading
import-export-style: leading
indent-wheres: true
record-brace-space: true
newlines-between-decls: 1
haddock-style: single-line
respectful: true
```

### Stylish Haskell

Highly configurable traditional formatter:

```bash
hx fmt --formatter stylish
```

Create `.stylish-haskell.yaml`:

```yaml
steps:
  - imports:
      align: global
      list_align: after_alias
      pad_module_names: true
      long_list_align: inline
      empty_list_align: inherit
      list_padding: 4
      separate_lists: true
      space_surround: false
  - language_pragmas:
      style: vertical
      align: true
      remove_redundant: true
  - simple_align:
      cases: true
      top_level_patterns: true
      records: true
  - trailing_whitespace: {}

columns: 100
newline: native
```

### Configuration

Configure in `hx.toml`:

```toml
[fmt]
formatter = "ormolu"  # or "fourmolu", "stylish"
check = false         # Check only by default
exclude = ["generated/**"]
```

## Linting

### Basic Usage

Lint all files:

```bash
hx lint
```

Lint specific files:

```bash
hx lint src/MyModule.hs
```

Auto-fix safe suggestions:

```bash
hx lint --fix
```

### HLint Output

```bash
$ hx lint
src/MyLib.hs:15:5: Warning: Use fmap
Found:
  do x <- getLine
     return (f x)
Perhaps:
  f <$> getLine

src/MyLib.hs:23:1: Suggestion: Eta reduce
Found:
  foo x = bar x
Perhaps:
  foo = bar

2 hints
```

### Auto-Fix

Apply safe fixes automatically:

```bash
hx lint --fix
```

Only suggestions marked as safe are applied.

### Ignoring Hints

#### In Configuration

`hx.toml`:

```toml
[lint]
ignore = ["Eta reduce", "Use fmap"]
```

#### In Source Code

```haskell
{-# HLINT ignore "Use fmap" #-}

foo = bar  {- HLINT ignore -}
```

#### Via .hlint.yaml

```yaml
- ignore: {name: "Use fmap"}
- ignore: {name: "Eta reduce", within: [Tests]}
```

### Custom Rules

Create custom hints in `.hlint.yaml`:

```yaml
# Warn about specific patterns
- warn: {lhs: "map head (map tail x)", rhs: "map (head . tail) x"}

# Enforce qualified imports
- modules:
  - {name: Data.Map.Strict, as: Map}
  - {name: Data.Set, as: Set}

# Custom suggestions
- suggest:
    name: Use Text.pack
    lhs: Data.Text.pack (show x)
    rhs: TextShow.showt x
```

### Configuration

Configure in `hx.toml`:

```toml
[lint]
extra-hints = [".hlint.yaml"]
ignore = [
  "Use camelCase",
  "Reduce duplication"
]
```

## CI Integration

### Format Check

```yaml
- name: Check formatting
  run: hx fmt --check
```

### Lint Check

```yaml
- name: Lint
  run: hx lint
```

### Combined

```yaml
- name: Code quality
  run: |
    hx fmt --check
    hx lint
```

## Pre-Commit Hooks

### Simple Hook

```bash
#!/bin/sh
# .git/hooks/pre-commit

hx fmt --check
if [ $? -ne 0 ]; then
    echo "Run 'hx fmt' to fix formatting"
    exit 1
fi

hx lint
```

### With Husky

`.husky/pre-commit`:

```bash
#!/bin/sh
hx fmt --check && hx lint
```

### With pre-commit Framework

`.pre-commit-config.yaml`:

```yaml
repos:
  - repo: local
    hooks:
      - id: hx-fmt
        name: Format Haskell
        entry: hx fmt --check
        language: system
        types: [haskell]
      - id: hx-lint
        name: Lint Haskell
        entry: hx lint
        language: system
        types: [haskell]
```

## Editor Integration

### VS Code

Install "Haskell" extension and configure:

```json
{
  "haskell.formattingProvider": "ormolu",
  "editor.formatOnSave": true,
  "[haskell]": {
    "editor.defaultFormatter": "haskell.haskell"
  }
}
```

### Vim/Neovim

With ALE:

```vim
let g:ale_fixers = {'haskell': ['ormolu']}
let g:ale_linters = {'haskell': ['hlint']}
let g:ale_fix_on_save = 1
```

### Emacs

```elisp
(use-package haskell-mode
  :hook (haskell-mode . ormolu-format-on-save-mode))

(use-package flycheck
  :hook (haskell-mode . flycheck-mode)
  :config
  (setq flycheck-haskell-hlint-executable "hlint"))
```

## Best Practices

### 1. Choose One Formatter

Stick with one formatter project-wide:

```toml
[fmt]
formatter = "ormolu"
```

### 2. Format on Save

Configure your editor to format on save for immediate feedback.

### 3. Run in CI

Always check formatting and linting in CI:

```yaml
- run: hx fmt --check && hx lint
```

### 4. Review Lint Suggestions

Not all suggestions are improvements. Use `ignore` judiciously.

### 5. Custom Hints for Project Style

Add project-specific rules:

```yaml
# .hlint.yaml
- warn: {lhs: "undefined", rhs: "error \"TODO\""}
```

## See Also

- [hx fmt](/docs/commands/fmt) — Format command
- [hx lint](/docs/commands/lint) — Lint command
- [Ormolu](https://github.com/tweag/ormolu) — Formatter docs
- [HLint](https://github.com/ndmitchell/hlint) — Linter docs
