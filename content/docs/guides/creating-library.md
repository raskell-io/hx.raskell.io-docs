+++
title = "Creating a Library"
weight = 5
+++

Guide to creating and publishing a Haskell library with hx.

## Create the Project

```bash
hx new my-lib --lib
cd my-lib
```

This creates:

```
my-lib/
├── src/
│   └── MyLib.hs
├── test/
│   └── Spec.hs
├── my-lib.cabal
├── hx.toml
├── CHANGELOG.md
└── README.md
```

## Library Structure

### Module Organization

```
src/
├── MyLib.hs              # Main module (re-exports)
└── MyLib/
    ├── Core.hs           # Core functionality
    ├── Parser.hs         # Parsing utilities
    ├── Types.hs          # Type definitions
    └── Internal.hs       # Internal (not exported)
```

### Main Module

```haskell
-- src/MyLib.hs
module MyLib
  ( -- * Types
    Config(..)
  , Result(..)
    -- * Core Functions
  , process
  , validate
    -- * Utilities
  , helper
  ) where

import MyLib.Core
import MyLib.Types
```

### Internal Modules

```haskell
-- src/MyLib/Internal.hs
module MyLib.Internal where

-- Not exported from main module
-- Used by other modules internally
```

## Cabal Configuration

### Basic Setup

```cabal
cabal-version: 3.0
name:          my-lib
version:       0.1.0.0
license:       MIT
license-file:  LICENSE
author:        Your Name
maintainer:    you@example.com
synopsis:      A useful library for doing things
description:
  A longer description of what your library does.
  Can span multiple lines.
category:      Development
extra-doc-files: CHANGELOG.md

source-repository head
  type:     git
  location: https://github.com/user/my-lib

common warnings
  ghc-options: -Wall -Wcompat

library
  import:           warnings
  exposed-modules:
    MyLib
    MyLib.Core
    MyLib.Types
  other-modules:
    MyLib.Internal
  hs-source-dirs:   src
  build-depends:
    , base ^>=4.18
    , text ^>=2.0
    , containers ^>=0.6
  default-language: GHC2021
  default-extensions:
    OverloadedStrings
    DeriveGeneric

test-suite my-lib-test
  import:           warnings
  type:             exitcode-stdio-1.0
  main-is:          Spec.hs
  other-modules:
    MyLib.CoreSpec
    MyLib.ParserSpec
  hs-source-dirs:   test
  build-depends:
    , base
    , my-lib
    , hspec ^>=2.11
    , QuickCheck ^>=2.14
  default-language: GHC2021
```

### Version Bounds

Use PVP-compatible bounds:

```cabal
build-depends:
  -- Allow patches, block minor
  , text ^>=2.0

  -- Allow minor, block major
  , aeson >=2.0 && <3

  -- Exact version (rarely needed)
  , specific-package ==1.2.3
```

## Writing Documentation

### Haddock Comments

```haskell
-- | Process the input data.
--
-- Takes a 'Config' and produces a 'Result'.
--
-- ==== Examples
--
-- >>> process defaultConfig "input"
-- Right (Result "output")
--
-- >>> process badConfig ""
-- Left ConfigError
process :: Config -> Text -> Either Error Result
process = ...
```

### Module Documentation

```haskell
{-|
Module      : MyLib.Core
Description : Core functionality for MyLib
Copyright   : (c) Your Name, 2024
License     : MIT
Maintainer  : you@example.com
Stability   : experimental

This module provides the core 'process' function
and related utilities.
-}
module MyLib.Core where
```

### Generate Documentation

```bash
hx doc --open
```

## Testing

### Test Structure

```haskell
-- test/Spec.hs
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
```

```haskell
-- test/MyLib/CoreSpec.hs
module MyLib.CoreSpec where

import Test.Hspec
import Test.QuickCheck
import MyLib.Core

spec :: Spec
spec = do
  describe "process" $ do
    it "handles valid input" $ do
      process defaultConfig "valid" `shouldBe` Right expected

    it "rejects empty input" $ do
      process defaultConfig "" `shouldSatisfy` isLeft

    it "is idempotent" $ property $ \input ->
      process cfg (process cfg input) == process cfg input
```

### Run Tests

```bash
hx test
hx test --match "Core"
hx test -- --quickcheck-tests 1000
```

## Version Management

### Semantic Versioning

Follow PVP (Package Versioning Policy):

```
A.B.C.D
│ │ │ └─ Patch: bug fixes
│ │ └─── Minor: backwards-compatible additions
│ └───── Major: breaking changes (type signatures)
└─────── Major: breaking changes (module structure)
```

### Updating Version

1. Update in `.cabal`:
   ```cabal
   version: 0.2.0.0
   ```

2. Update CHANGELOG.md:
   ```markdown
   ## 0.2.0.0 - 2024-01-15

   ### Added
   - New `parseStrict` function

   ### Changed
   - `process` now returns `Either Error Result`
   ```

## Publishing to Hackage

### Prerequisites

1. Create [Hackage account](https://hackage.haskell.org/users/register)
2. Get upload credentials

### Pre-Publish Checklist

```bash
# Verify everything builds
hx build --release

# Run all tests
hx test

# Check documentation
hx doc --open

# Lint the code
hx lint

# Check package
cabal check
```

### Create Source Distribution

```bash
cabal sdist
```

Creates `dist-newstyle/sdist/my-lib-0.1.0.0.tar.gz`

### Upload

```bash
# Candidate (for review)
cabal upload dist-newstyle/sdist/my-lib-0.1.0.0.tar.gz

# Final publish
cabal upload --publish dist-newstyle/sdist/my-lib-0.1.0.0.tar.gz
```

## Best Practices

### 1. Minimize Dependencies

Only add essential dependencies:

```cabal
build-depends:
  , base ^>=4.18
  -- Only what you really need
```

### 2. Export Thoughtfully

```haskell
module MyLib
  ( -- Only export stable API
    Config(..)
  , process
  -- Don't export: internalHelper
  ) where
```

### 3. Use Internal Modules

```cabal
exposed-modules: MyLib
other-modules: MyLib.Internal
```

### 4. Provide Examples

```haskell
-- | Example usage:
--
-- @
-- import MyLib
--
-- main = do
--   result <- process defaultConfig input
--   print result
-- @
```

### 5. Maintain Changelog

Keep CHANGELOG.md updated with every release.

## See Also

- [hx new](/docs/commands/new) — Create projects
- [hx doc](/docs/commands/doc) — Generate documentation
- [Hackage](https://hackage.haskell.org/) — Package repository
