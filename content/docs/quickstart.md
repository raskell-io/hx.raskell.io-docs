+++
title = "Quick Start"
weight = 2
+++

This guide will walk you through creating and building your first Haskell project with hx.

## Prerequisites

Before starting, make sure you have:
1. [hx installed](/docs/installation)
2. A Haskell toolchain (GHC + Cabal)

If you don't have a Haskell toolchain, install it with:

```bash
hx toolchain install
```

## Create a New Project

Create a new Haskell project with sensible defaults:

```bash
hx new my-project
cd my-project
```

This creates a project with the following structure:

```
my-project/
├── app/
│   └── Main.hs          # Application entry point
├── src/
│   └── MyProject.hs     # Library source
├── test/
│   └── Spec.hs          # Test suite
├── my-project.cabal     # Cabal package description
├── hx.toml              # hx configuration
└── README.md
```

### Project Templates

hx supports different project templates:

```bash
# Default: library + executable + tests
hx new my-project

# Library only
hx new my-lib --lib

# Executable only
hx new my-app --exe

# Minimal setup
hx new my-minimal --minimal
```

## Build Your Project

Compile your project:

```bash
hx build
```

For optimized release builds:

```bash
hx build --release
```

Build output is placed in the `dist-newstyle` directory (Cabal's default).

## Run Your Project

Run the executable:

```bash
hx run
```

Pass arguments to your program:

```bash
hx run -- --help
hx run -- arg1 arg2
```

For projects with multiple executables:

```bash
hx run --exe my-other-exe
```

## Add Dependencies

Add packages to your project:

```bash
# Add a single package
hx add text

# Add multiple packages
hx add text containers aeson

# Add with version constraint
hx add "aeson >=2.0 && <2.2"

# Add as dev dependency (test/benchmark only)
hx add --dev hspec QuickCheck
```

Dependencies are added to your `.cabal` file automatically.

## Generate Lockfile

Create a lockfile for reproducible builds:

```bash
hx lock
```

This creates `hx.lock` with pinned versions of all dependencies.

## Run Tests

Execute your test suite:

```bash
hx test
```

Run specific tests:

```bash
# Run tests matching a pattern
hx test --match "Parser"

# Run a specific test component
hx test --test my-project-test
```

## Start the REPL

Launch an interactive GHCi session with your project loaded:

```bash
hx repl
```

The REPL loads your library modules, making them available for interactive exploration:

```haskell
ghci> import MyProject
ghci> :type someFunction
```

## Type Check Without Building

Quickly check your code for errors without a full build:

```bash
hx check
```

This is faster than `hx build` and is useful during development.

## Format and Lint

Format your Haskell code:

```bash
hx fmt
```

Run the linter:

```bash
hx lint
```

## Watch Mode

Automatically rebuild when files change:

```bash
# Watch and rebuild
hx watch build

# Watch and run tests
hx watch test

# Watch and run the project
hx watch run
```

## Check Your Environment

Diagnose toolchain issues:

```bash
hx doctor
```

This checks for:
- GHC installation and version
- Cabal installation
- GHCup availability
- HLS compatibility
- System dependencies

## What's Next?

- Learn about [all available commands](/docs/commands)
- Configure your project with [hx.toml](/docs/configuration/hx-toml)
- Explore [advanced features](/docs/features)
- Read the [workflow guides](/docs/guides)
