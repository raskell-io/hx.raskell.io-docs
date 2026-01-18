+++
title = "hx run"
weight = 2
+++

Build and run the project executable.

## Synopsis

```bash
hx run [OPTIONS] [-- <ARGS>...]
```

## Description

The `run` command compiles your project (if needed) and executes the resulting binary. Arguments after `--` are passed to your program.

## Options

### Run Options

```
    --exe <NAME>        Run a specific executable
    --release           Build with optimizations before running
    --no-build          Skip build step (run existing binary)
```

### Compiler Options

```
    --backend <BACKEND>    Compiler backend [ghc, bhc]
    --target <TARGET>      Cross-compilation target
```

### Output Options

```
-v, --verbose           Show build output
-q, --quiet             Suppress hx output, only show program output
```

## Examples

### Basic Run

```bash
# Build and run the default executable
hx run
```

### Pass Arguments to Your Program

```bash
# Pass arguments after --
hx run -- --help
hx run -- input.txt --output=result.json
hx run -- arg1 arg2 arg3
```

### Run Specific Executable

For projects with multiple executables:

```bash
# Run a specific executable
hx run --exe my-cli

# With arguments
hx run --exe server -- --port 8080
```

### Release Mode

```bash
# Build optimized version and run
hx run --release
```

### Skip Build

```bash
# Run without rebuilding (use existing binary)
hx run --no-build
```

### Using BHC Backend

```bash
# Run using the BHC compiler backend
hx run --backend bhc
```

## Configuration

Configure default run behavior in `hx.toml`:

```toml
[run]
default-exe = "my-app"     # Default executable to run
args = ["--verbose"]       # Default arguments

[build]
release = false            # Debug builds by default
```

## Multiple Executables

If your project defines multiple executables in the `.cabal` file:

```cabal
executable my-app
  main-is: Main.hs
  hs-source-dirs: app

executable my-cli
  main-is: CLI.hs
  hs-source-dirs: cli

executable my-server
  main-is: Server.hs
  hs-source-dirs: server
```

You can run each with:

```bash
hx run --exe my-app
hx run --exe my-cli
hx run --exe my-server
```

## Interactive Programs

For interactive programs that read from stdin:

```bash
# Run and allow interactive input
hx run

# Pipe input
echo "input" | hx run

# Redirect from file
hx run < input.txt
```

## Environment Variables

Environment variables are passed through to your program:

```bash
# Set environment variables
MY_VAR=value hx run

# Or export first
export DATABASE_URL="postgres://..."
hx run
```

## Exit Codes

The exit code from `hx run` is the exit code of your program, unless there's a build error:

| Code | Meaning |
|------|---------|
| 0 | Program succeeded |
| 1-255 | Program exit code |
| 5 | Build failure (before running) |

## See Also

- [hx build](/docs/commands/build) — Compile without running
- [hx test](/docs/commands/test) — Run test suite
- [hx watch run](/docs/commands/watch) — Auto-run on file changes
