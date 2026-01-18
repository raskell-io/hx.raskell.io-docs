+++
title = "hx doc"
weight = 7
+++

Generate documentation.

## Synopsis

```bash
hx doc [OPTIONS]
```

## Description

The `doc` command generates Haddock documentation for your project. It creates HTML documentation that can be viewed in a browser.

## Options

```
    --open              Open documentation in browser after building
    --deps              Also document dependencies
    --internal          Include internal modules
    --no-hoogle         Skip Hoogle database generation
-v, --verbose           Show detailed output
```

## Examples

### Generate Documentation

```bash
hx doc
```

### Open in Browser

```bash
hx doc --open
```

### Include Dependencies

```bash
hx doc --deps
```

### Include Internal Modules

```bash
hx doc --internal
```

## Writing Documentation

### Module Documentation

```haskell
{-|
Module      : MyProject.Parser
Description : JSON parsing utilities
Copyright   : (c) Your Name, 2024
License     : MIT
Maintainer  : you@example.com
Stability   : experimental

This module provides JSON parsing functionality
with support for streaming and error recovery.
-}
module MyProject.Parser
  ( parse
  , parseFile
  , ParseError(..)
  ) where
```

### Function Documentation

```haskell
-- | Parse a JSON string.
--
-- Returns a 'ParseError' if the input is invalid.
--
-- ==== Examples
--
-- >>> parse "{\"key\": \"value\"}"
-- Right (Object [("key", String "value")])
--
-- >>> parse "invalid"
-- Left (ParseError "unexpected character")
parse :: String -> Either ParseError Value
parse = ...
```

### Data Type Documentation

```haskell
-- | Configuration for the parser.
data ParserConfig = ParserConfig
  { maxDepth :: !Int
    -- ^ Maximum nesting depth (default: 100)
  , allowComments :: !Bool
    -- ^ Allow JavaScript-style comments (default: False)
  , strictMode :: !Bool
    -- ^ Enable strict JSON compliance (default: True)
  }
```

### Section Headers

```haskell
-- * Parsing functions
-- $parsing
--
-- Functions for parsing JSON data.

-- | Parse a string
parse :: String -> Value

-- | Parse a file
parseFile :: FilePath -> IO Value

-- * Error handling
-- $errors
--
-- Error types and handling utilities.

-- | Parse error type
data ParseError = ...
```

## Configuration

Configure documentation in `hx.toml`:

```toml
[doc]
# Include internal modules
internal = false

# Haddock options
haddock-options = ["--hyperlinked-source"]

# Output directory
output-dir = "docs"
```

## Documentation Output

Documentation is generated in:

```
dist-newstyle/build/<platform>/ghc-<version>/<package>/doc/html/<package>/
```

Use `--open` to automatically open the index.html file.

## Haddock Markup

### Links

```haskell
-- | See 'otherFunction' for details.
-- | Uses "Data.Text" internally.
-- | Reference to 'Module.Name.function'.
```

### Code Blocks

```haskell
-- | Example:
--
-- @
-- result = parse input
-- case result of
--   Left err -> handleError err
--   Right val -> process val
-- @
```

### Lists

```haskell
-- | Supported formats:
--
-- * JSON
-- * YAML
-- * TOML
--
-- Or numbered:
--
-- 1. Parse input
-- 2. Validate structure
-- 3. Return result
```

### Properties (for testing)

```haskell
-- | Reverse is involutive.
--
-- prop> reverse (reverse xs) == (xs :: [Int])
reverse :: [a] -> [a]
```

## CI Integration

Generate docs in CI:

```yaml
- name: Build documentation
  run: hx doc

- name: Upload docs
  uses: actions/upload-artifact@v3
  with:
    name: documentation
    path: dist-newstyle/build/**/doc/html/
```

## See Also

- [hx build](/docs/commands/build) — Build the project
- [Haddock documentation](https://haskell-haddock.readthedocs.io/)
