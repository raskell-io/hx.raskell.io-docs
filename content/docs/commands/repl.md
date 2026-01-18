+++
title = "hx repl"
weight = 6
+++

Start an interactive GHCi session.

## Synopsis

```bash
hx repl [OPTIONS] [COMPONENT]
```

## Description

The `repl` command starts GHCi with your project loaded. You can interactively explore your code, test functions, and experiment with ideas.

## Options

```
    --no-build          Don't rebuild before starting REPL
    --ghci-options      Additional options to pass to GHCi
-v, --verbose           Show REPL startup details
```

## Arguments

```
COMPONENT              Component to load (lib, exe:name, test:name)
```

## Examples

### Start REPL with Library

```bash
# Load the library
hx repl
```

### Load Specific Component

```bash
# Load an executable
hx repl exe:my-app

# Load tests
hx repl test:my-tests
```

### Skip Rebuild

```bash
hx repl --no-build
```

### Custom GHCi Options

```bash
hx repl --ghci-options="-XOverloadedStrings -XDeriveGeneric"
```

## Using the REPL

Once in the REPL, you can:

### Import Your Modules

```haskell
ghci> import MyProject
ghci> import MyProject.Parser
```

### Test Functions

```haskell
ghci> parse "test input"
Right (ParsedData {...})

ghci> process [1, 2, 3]
[2, 4, 6]
```

### Check Types

```haskell
ghci> :type parse
parse :: String -> Either ParseError ParsedData

ghci> :info Maybe
data Maybe a = Nothing | Just a
```

### Browse Modules

```haskell
ghci> :browse MyProject
parse :: String -> Either ParseError ParsedData
process :: [Int] -> [Int]
Config(..)
```

### Reload Changes

```haskell
ghci> :reload
```

### Useful GHCi Commands

| Command | Description |
|---------|-------------|
| `:reload` / `:r` | Reload modified modules |
| `:type expr` / `:t` | Show expression type |
| `:info name` / `:i` | Info about name |
| `:browse Module` | List module exports |
| `:quit` / `:q` | Exit GHCi |
| `:help` / `:?` | Show help |

## Configuration

Configure REPL behavior in `hx.toml`:

```toml
[repl]
# Default component to load
default = "lib"

# Load these modules automatically
auto-load = ["MyProject", "MyProject.Prelude"]

# GHCi options
ghci-options = ["-XOverloadedStrings"]

# Custom .ghci file
ghci-conf = ".ghci"
```

## Custom .ghci File

Create a `.ghci` file in your project root:

```haskell
-- .ghci
:set prompt "λ> "
:set prompt-cont "| "
:set -XOverloadedStrings
:set -XDeriveGeneric
import Data.Text (Text)
```

## REPL Workflows

### Exploratory Development

1. Start REPL: `hx repl`
2. Write functions in your source files
3. Reload: `:reload`
4. Test interactively
5. Repeat

### Testing Ideas

```haskell
-- Test a function idea quickly
ghci> let quickSort [] = []
ghci>     quickSort (x:xs) = quickSort smaller ++ [x] ++ quickSort larger
ghci>       where smaller = filter (< x) xs
ghci>             larger = filter (>= x) xs
ghci> quickSort [3, 1, 4, 1, 5, 9, 2, 6]
[1,1,2,3,4,5,6,9]
```

### Debugging

```haskell
ghci> import Debug.Trace
ghci> :set -fbreak-on-exception
ghci> myFunction input
*** Exception: ...
ghci> :back
ghci> :list
```

## See Also

- [hx build](/docs/commands/build) — Compile the project
- [hx check](/docs/commands/check) — Type check without compiling
