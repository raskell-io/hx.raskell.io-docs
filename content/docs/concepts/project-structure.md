+++
title = "Project Structure"
weight = 1
+++

Understanding how hx projects are organized.

## Standard Layout

A typical hx project looks like:

```
my-project/
├── app/
│   └── Main.hs           # Application entry point
├── src/
│   └── MyProject.hs      # Library source
│   └── MyProject/
│       ├── Parser.hs
│       └── Types.hs
├── test/
│   └── Spec.hs           # Test suite
├── bench/
│   └── Main.hs           # Benchmarks (optional)
├── dist-newstyle/        # Build artifacts (gitignored)
├── .hx/                  # hx metadata (gitignored)
├── my-project.cabal      # Cabal package description
├── hx.toml               # hx configuration
├── hx.lock               # Dependency lockfile
├── CHANGELOG.md
└── README.md
```

## Key Files

### hx.toml

Project configuration for hx:

```toml
[project]
name = "my-project"
version = "0.1.0"

[toolchain]
ghc = "9.8.2"

[build]
ghc-options = ["-Wall"]
```

### my-project.cabal

Cabal package description (source of truth for:
- Package metadata
- Dependencies
- Component definitions

```cabal
cabal-version: 3.0
name:          my-project
version:       0.1.0

library
  exposed-modules: MyProject
  hs-source-dirs:  src
  build-depends:   base ^>=4.18

executable my-project
  main-is:       Main.hs
  hs-source-dirs: app
  build-depends:
    , base ^>=4.18
    , my-project
```

### hx.lock

Lockfile with pinned dependency versions:

```toml
version = 1
generated = "2024-01-15T10:30:00Z"

[[package]]
name = "text"
version = "2.0.2"
sha256 = "..."
```

## Source Directories

### src/

Library source code:

```
src/
└── MyProject.hs           # MyProject module
└── MyProject/
    ├── Parser.hs          # MyProject.Parser
    ├── Types.hs           # MyProject.Types
    └── Internal.hs        # MyProject.Internal (not exported)
```

Module naming convention:
- Top-level: `src/Foo.hs` → `Foo`
- Nested: `src/Foo/Bar.hs` → `Foo.Bar`

### app/

Executable source code:

```
app/
└── Main.hs                # main entry point
```

For multiple executables:

```
app/
├── Main.hs                # my-project executable
└── CLI.hs                 # my-project-cli executable
```

### test/

Test code:

```
test/
├── Spec.hs                # Test entry point
├── MyProject/
│   ├── ParserSpec.hs      # Parser tests
│   └── TypesSpec.hs       # Types tests
└── TestUtils.hs           # Test utilities
```

### bench/

Benchmark code:

```
bench/
└── Main.hs                # Benchmark entry point
```

## Build Artifacts

### dist-newstyle/

Cabal build directory (gitignored):

```
dist-newstyle/
├── build/                 # Compiled objects
│   └── x86_64-osx/
│       └── ghc-9.8.2/
│           └── my-project-0.1.0/
├── cache/                 # Cabal cache
├── packagedb/             # Local package database
└── tmp/                   # Temporary files
```

### .hx/

hx-specific metadata (gitignored):

```
.hx/
├── cache/                 # hx cache
├── bhc.toml               # Generated BHC manifest (if using BHC)
└── bhc-reports/           # BHC optimization reports
```

## Configuration Files

### .gitignore

Standard gitignore:

```gitignore
# Build artifacts
dist-newstyle/
dist/
.cabal-sandbox/
cabal.sandbox.config

# hx
.hx/

# Editor
*.swp
*~
.vscode/
.idea/

# GHC
*.hi
*.o
*.dyn_hi
*.dyn_o
```

### .hlint.yaml

HLint configuration:

```yaml
- ignore: {name: "Use camelCase"}
- modules:
  - {name: Data.Map.Strict, as: Map}
```

### fourmolu.yaml

Fourmolu configuration (if used):

```yaml
indentation: 2
function-arrows: leading
```

## Project Variants

### Library Only

```
my-lib/
├── src/
│   └── MyLib.hs
├── test/
│   └── Spec.hs
├── my-lib.cabal
└── hx.toml
```

### Executable Only

```
my-app/
├── app/
│   └── Main.hs
├── my-app.cabal
└── hx.toml
```

### Multi-Package (Workspace)

```
my-workspace/
├── packages/
│   ├── core/
│   │   ├── src/
│   │   └── core.cabal
│   ├── cli/
│   │   ├── app/
│   │   └── cli.cabal
│   └── web/
│       ├── src/
│       └── web.cabal
├── cabal.project
├── hx.toml
└── hx.lock
```

## Cabal Components

### Library

```cabal
library
  exposed-modules:
    MyProject
    MyProject.Parser
  other-modules:
    MyProject.Internal
  hs-source-dirs: src
  build-depends:
    , base ^>=4.18
    , text ^>=2.0
```

### Executable

```cabal
executable my-project
  main-is: Main.hs
  hs-source-dirs: app
  build-depends:
    , base ^>=4.18
    , my-project
```

### Test Suite

```cabal
test-suite my-project-test
  type: exitcode-stdio-1.0
  main-is: Spec.hs
  hs-source-dirs: test
  build-depends:
    , base ^>=4.18
    , my-project
    , hspec ^>=2.11
```

### Benchmark

```cabal
benchmark my-project-bench
  type: exitcode-stdio-1.0
  main-is: Main.hs
  hs-source-dirs: bench
  build-depends:
    , base ^>=4.18
    , my-project
    , criterion ^>=1.6
```

## Best Practices

### 1. Separate Library and Executable

Put reusable code in the library:

```haskell
-- src/MyProject.hs
module MyProject (run) where

run :: IO ()
run = putStrLn "Hello"
```

Keep executable thin:

```haskell
-- app/Main.hs
module Main where

import MyProject (run)

main :: IO ()
main = run
```

### 2. Use Internal Modules

Hide implementation details:

```cabal
library
  exposed-modules: MyProject
  other-modules: MyProject.Internal
```

### 3. Consistent Naming

- Module names match directory structure
- Test modules mirror source modules
- Descriptive executable names

### 4. Keep Root Clean

Minimize files in project root. Use subdirectories for organization.

## See Also

- [hx new](/docs/commands/new) — Create new project
- [hx init](/docs/commands/init) — Initialize existing project
