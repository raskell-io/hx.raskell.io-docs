+++
title = "hx new"
weight = 10
+++

Create a new Haskell project.

## Synopsis

```bash
hx new <NAME> [OPTIONS]
```

## Description

The `new` command creates a new Haskell project with a complete directory structure, Cabal configuration, and hx configuration file.

## Arguments

```
NAME                    Name of the project (must be a valid Cabal package name)
```

## Options

### Project Structure

```
    --lib               Create a library-only project
    --exe               Create an executable-only project
    --minimal           Create minimal project structure
    --no-tests          Skip test suite creation
    --no-git            Don't initialize git repository
```

### Configuration

```
    --ghc <VERSION>     GHC version to use (e.g., "9.8.2")
    --backend <BACKEND> Compiler backend [ghc, bhc]
    --language <STD>    Haskell language standard [Haskell2010, GHC2021, GHC2024]
```

### Metadata

```
    --author <NAME>     Author name
    --email <EMAIL>     Author email
    --license <LICENSE> License type (MIT, BSD3, Apache-2.0, etc.)
```

## Examples

### Create Default Project

```bash
hx new my-project
```

Creates:

```
my-project/
├── app/
│   └── Main.hs
├── src/
│   └── MyProject.hs
├── test/
│   └── Spec.hs
├── my-project.cabal
├── hx.toml
├── CHANGELOG.md
├── README.md
└── .gitignore
```

### Library Only

```bash
hx new my-lib --lib
```

Creates a library without an executable:

```
my-lib/
├── src/
│   └── MyLib.hs
├── test/
│   └── Spec.hs
├── my-lib.cabal
├── hx.toml
└── README.md
```

### Executable Only

```bash
hx new my-app --exe
```

Creates an executable without a library:

```
my-app/
├── app/
│   └── Main.hs
├── my-app.cabal
├── hx.toml
└── README.md
```

### Minimal Project

```bash
hx new my-minimal --minimal
```

Creates the bare minimum:

```
my-minimal/
├── Main.hs
├── my-minimal.cabal
└── hx.toml
```

### With Specific GHC Version

```bash
hx new my-project --ghc 9.8.2
```

### With BHC Backend

```bash
hx new ml-project --backend bhc
```

### With Author Information

```bash
hx new my-project \
  --author "Your Name" \
  --email "you@example.com" \
  --license MIT
```

### Without Git

```bash
hx new my-project --no-git
```

## Generated Files

### hx.toml

```toml
[project]
name = "my-project"
version = "0.1.0"

[toolchain]
ghc = "9.8.2"

[build]
ghc-options = ["-Wall"]
```

### .cabal file

```cabal
cabal-version: 3.0
name:          my-project
version:       0.1.0
license:       MIT
author:        Your Name
maintainer:    you@example.com

common warnings
  ghc-options: -Wall

library
  import:           warnings
  exposed-modules:  MyProject
  hs-source-dirs:   src
  build-depends:    base ^>=4.18
  default-language: GHC2021

executable my-project
  import:           warnings
  main-is:          Main.hs
  hs-source-dirs:   app
  build-depends:
    , base ^>=4.18
    , my-project
  default-language: GHC2021

test-suite my-project-test
  import:           warnings
  type:             exitcode-stdio-1.0
  main-is:          Spec.hs
  hs-source-dirs:   test
  build-depends:
    , base ^>=4.18
    , my-project
    , hspec
  default-language: GHC2021
```

### Main.hs

```haskell
module Main where

import MyProject (someFunc)

main :: IO ()
main = someFunc
```

### Library Module

```haskell
module MyProject (someFunc) where

someFunc :: IO ()
someFunc = putStrLn "Hello from my-project!"
```

## Package Naming

Package names must:
- Start with a letter
- Contain only letters, digits, and hyphens
- Not end with a hyphen

Valid: `my-project`, `parser2`, `json-utils`
Invalid: `2parser`, `my--project`, `project-`

## Post-Creation

After creating a project:

```bash
cd my-project

# Build
hx build

# Run
hx run

# Test
hx test
```

## See Also

- [hx init](/docs/commands/init) — Initialize hx in existing project
- [Quick Start](/docs/quickstart) — Getting started guide
