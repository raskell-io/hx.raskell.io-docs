+++
title = "hx lint"
weight = 42
+++

Run the Haskell linter.

## Synopsis

```bash
hx lint [OPTIONS] [FILES]...
```

## Description

The `lint` command runs HLint on your Haskell source files, identifying potential improvements and style issues.

## Arguments

```
FILES...                Files to lint (default: all .hs files)
```

## Options

```
    --fix               Auto-apply safe suggestions
    --hint <FILE>       Additional hint file
    --ignore <HINT>     Ignore specific hints
    --json              Output as JSON
    --report <FILE>     Generate HTML report
-v, --verbose           Show detailed output
```

## Examples

### Lint All Files

```bash
hx lint
```

### Lint Specific Files

```bash
hx lint src/MyModule.hs
```

### Auto-Fix

```bash
hx lint --fix
```

### Ignore Hints

```bash
hx lint --ignore "Use newtype instead of data"
```

### JSON Output

```bash
hx lint --json
```

### HTML Report

```bash
hx lint --report lint-report.html
```

## Sample Output

```
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

## Hint Categories

### Error

Must be fixed:
```
Use otherwise, not True
```

### Warning

Should be fixed:
```
Use fmap
Use <$>
Redundant bracket
```

### Suggestion

Consider fixing:
```
Eta reduce
Use when
Use unless
```

## Auto-Fix

`hx lint --fix` automatically applies safe suggestions:

```bash
hx lint --fix
```

Before:
```haskell
foo x = bar x
```

After:
```haskell
foo = bar
```

Only suggestions marked as safe are applied automatically.

## Configuration

### hx.toml

```toml
[lint]
# HLint options
extra-hints = [".hlint.yaml"]

# Ignored hints
ignore = [
  "Use camelCase",
  "Reduce duplication"
]
```

### .hlint.yaml

Create custom hints:

```yaml
# Ignore specific warnings
- ignore: {name: "Use fmap"}
- ignore: {name: "Eta reduce", within: [Tests]}

# Custom hints
- warn: {lhs: "map head (map tail x)", rhs: "map (head . tail) x"}

# Module-specific rules
- modules:
  - {name: Data.Map.Strict, as: Map}
  - {name: Data.Set, as: Set}
```

## Ignoring Hints

### In Source Code

```haskell
{-# HLINT ignore "Use fmap" #-}

-- or for specific expression
foo = bar  {- HLINT ignore -}
```

### In Configuration

```yaml
# .hlint.yaml
- ignore: {name: "Use fmap"}
- ignore: {name: "Redundant bracket", within: [ParserModule]}
```

### Via Command Line

```bash
hx lint --ignore "Use fmap" --ignore "Eta reduce"
```

## Common Hints

| Hint | Before | After |
|------|--------|-------|
| Use fmap | `do x <- m; return (f x)` | `f <$> m` |
| Use <$> | `fmap f m` | `f <$> m` |
| Eta reduce | `\x -> f x` | `f` |
| Use when | `if b then m else pure ()` | `when b m` |
| Use unless | `if b then pure () else m` | `unless b m` |
| Redundant bracket | `(f x)` | `f x` |

## CI Integration

```yaml
- name: Lint
  run: hx lint

# Or fail on warnings
- name: Lint (strict)
  run: hx lint --json | jq '.[] | select(.severity == "Warning")'
```

## Editor Integration

### VS Code

HLint integrations show hints inline.

### Vim

With ALE:

```vim
let g:ale_linters = {'haskell': ['hlint']}
```

## See Also

- [hx fmt](/docs/commands/fmt) — Format code
- [HLint documentation](https://github.com/ndmitchell/hlint)
