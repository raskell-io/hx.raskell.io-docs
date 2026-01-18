+++
title = "Development Workflow"
weight = 10
+++

Recommended workflows for day-to-day Haskell development with hx.

## Starting Your Day

### Check Environment

```bash
# Verify everything is set up
hx doctor

# Update package index periodically
cabal update
```

### Sync Dependencies

If you pulled changes:

```bash
git pull
hx sync
```

## Development Workflows

### Test-Driven Development

The TDD loop with watch mode:

```bash
hx watch test
```

Workflow:
1. Write a failing test
2. Save → tests run automatically
3. Write implementation
4. Save → tests pass
5. Refactor
6. Save → tests still pass

### Type-Driven Development

Fast type checking feedback:

```bash
hx watch check
```

Workflow:
1. Write type signatures first
2. Save → see type errors
3. Implement functions
4. Save → types check

### Interactive Development

REPL-driven workflow:

```bash
hx repl
```

```haskell
ghci> import MyModule
ghci> :type myFunction
ghci> myFunction testInput
-- Make changes to file
ghci> :reload
ghci> myFunction testInput
```

### Build and Run

For application development:

```bash
hx watch run
```

Automatically rebuilds and restarts on changes.

## Feature Development

### Starting a New Feature

```bash
# Create feature branch
git checkout -b feature/new-feature

# Add dependencies if needed
hx add new-package

# Update lockfile
hx lock

# Start development
hx watch test
```

### Adding a New Module

1. Create the file:
   ```bash
   touch src/MyProject/NewModule.hs
   ```

2. Add to `.cabal`:
   ```cabal
   exposed-modules:
     MyProject.NewModule
   ```

3. Implement:
   ```haskell
   module MyProject.NewModule where

   -- implementation
   ```

4. Add tests:
   ```haskell
   -- test/MyProject/NewModuleSpec.hs
   module MyProject.NewModuleSpec where

   import Test.Hspec
   import MyProject.NewModule

   spec :: Spec
   spec = do
     describe "newFunction" $ do
       it "does something" $ do
         -- test
   ```

### Adding Dependencies

```bash
# Add to project
hx add text containers

# Add test-only dependencies
hx add --dev hspec QuickCheck

# Update lockfile
hx lock

# Verify
hx build
hx test
```

## Code Quality

### Before Committing

```bash
# Format code
hx fmt

# Check for issues
hx lint

# Run tests
hx test

# Type check
hx check
```

### Using Pre-commit Hooks

Create `.git/hooks/pre-commit`:

```bash
#!/bin/sh
set -e

echo "Checking formatting..."
hx fmt --check

echo "Running linter..."
hx lint

echo "Running tests..."
hx test
```

Make executable:
```bash
chmod +x .git/hooks/pre-commit
```

## Debugging

### Adding Debug Output

```haskell
import Debug.Trace

myFunction x =
  trace ("myFunction called with: " ++ show x) $
  -- implementation
```

Run:
```bash
hx run
```

### Using GHCi Debugger

```bash
hx repl
```

```haskell
ghci> :set -fbreak-on-exception
ghci> myFunction input
*** Exception: ...
ghci> :back
ghci> :list
```

### Verbose Build Output

```bash
hx build --verbose
```

Shows full GHC command and output.

## Performance Work

### Profiling

```bash
# Build with profiling
hx build --ghc-options="-prof -fprof-auto"

# Run with profiling
hx run -- +RTS -p -RTS

# View profile
cat my-app.prof
```

### Benchmarking

```bash
# Always use release mode
hx bench --release
```

## Release Preparation

### Pre-Release Checklist

```bash
# Update version in .cabal
# Update CHANGELOG.md

# Ensure lockfile is current
hx lock

# Run full test suite
hx test

# Build release
hx build --release

# Generate docs
hx doc

# Check everything
hx doctor
```

### Creating a Release

```bash
# Commit changes
git add -A
git commit -m "Release v1.0.0"

# Tag
git tag v1.0.0

# Push
git push --tags
```

## Team Workflows

### Code Review

Before submitting PR:

```bash
hx fmt
hx lint
hx test
hx lock --check  # Ensure lockfile is current
```

### Updating Dependencies

```bash
# Check what's outdated
hx outdated

# Update specific packages
hx update aeson text

# Update all
hx update

# Run tests
hx test

# Commit lockfile changes
git add hx.lock
git commit -m "Update dependencies"
```

### Handling Merge Conflicts

If `hx.lock` conflicts:

```bash
# Take one side
git checkout --ours hx.lock

# Regenerate
hx lock

# Verify
hx build
hx test

# Commit
git add hx.lock
git commit
```

## Editor Integration

### VS Code

Install "Haskell" extension. Recommended settings:

```json
{
  "haskell.formattingProvider": "ormolu",
  "editor.formatOnSave": true
}
```

Use integrated terminal for:
```bash
hx watch test
```

### Vim/Neovim

Split workflow:
```vim
:split | terminal hx watch test
```

### Emacs

```elisp
(defun hx-watch-test ()
  (interactive)
  (async-shell-command "hx watch test"))
```

## See Also

- [hx watch](/docs/commands/watch) — Watch mode
- [hx repl](/docs/commands/repl) — Interactive development
- [Formatting & Linting](/docs/features/formatting-linting) — Code quality
