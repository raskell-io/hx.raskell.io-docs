+++
title = "hx check"
weight = 5
+++

Type check the project without building.

## Synopsis

```bash
hx check [OPTIONS]
```

## Description

The `check` command runs the type checker on your project without performing a full compilation. This is faster than `hx build` and is useful for quick feedback during development.

It's equivalent to running `cabal build --ghc-options="-fno-code"`.

## Options

```
    --lib               Check only the library
    --exe <NAME>        Check only the specified executable
    --test              Check only tests
    --all               Check all components
    --backend <BACKEND> Compiler backend [ghc, bhc]
-v, --verbose           Show detailed output
```

## Examples

### Check All Components

```bash
hx check
```

### Check Library Only

```bash
hx check --lib
```

### Check Specific Executable

```bash
hx check --exe my-app
```

### Check Tests

```bash
hx check --test
```

## When to Use Check vs Build

Use `hx check` when:
- You want fast feedback on type errors
- You're iterating on code design
- You don't need to run the resulting binary
- You're using an IDE/editor integration

Use `hx build` when:
- You need to run the program
- You want to catch linker errors
- You're preparing for deployment

## Performance

`hx check` is typically 2-5x faster than `hx build` because it:
- Skips code generation
- Skips object file creation
- Skips linking

## Integration with Editors

Many editors use `hx check` or similar for real-time feedback:

### VS Code with HLS

HLS provides real-time type checking. You can also run:

```bash
hx check
```

### Vim/Neovim

With ALE or coc.nvim:

```vim
" Use hx check for linting
let g:ale_linters = {'haskell': ['hx']}
```

## Configuration

```toml
[check]
# Enable all warnings
warnings = ["all"]

# Treat warnings as errors
werror = false
```

## See Also

- [hx build](/docs/commands/build) — Full compilation
- [hx watch](/docs/commands/watch) — Auto-check on file changes
