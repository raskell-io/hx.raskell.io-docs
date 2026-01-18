+++
title = "hx update"
weight = 14
+++

Update dependencies to newer versions.

## Synopsis

```bash
hx update [PACKAGES]... [OPTIONS]
```

## Description

The `update` command updates dependencies to their latest compatible versions. Without arguments, it updates all dependencies. With package names, it updates only those packages.

## Arguments

```
PACKAGES...             Specific packages to update (optional)
```

## Options

```
    --all               Update all dependencies
    --major             Allow major version updates
    --patch             Only patch version updates
    --dry-run           Show what would be updated
-v, --verbose           Show detailed output
```

## Examples

### Update All Dependencies

```bash
hx update
```

### Update Specific Packages

```bash
hx update aeson text
```

### Preview Updates

```bash
hx update --dry-run
```

Output:
```
Would update:
  aeson: 2.1.0.0 -> 2.2.0.0
  text: 2.0.1 -> 2.1
  containers: 0.6.7 -> 0.6.8
```

### Allow Major Updates

```bash
# By default, respects version constraints in .cabal
# Use --major to allow breaking changes
hx update --major aeson
```

### Patch Updates Only

```bash
# Only update patch versions (safest)
hx update --patch
```

## Update Strategies

### Default (Minor)

Updates within version constraints:

```
aeson ^>=2.1 allows 2.1.0.0 -> 2.1.2.0
aeson ^>=2.1 blocks 2.1.0.0 -> 2.2.0.0
```

### Patch Only

Only bug fixes:

```
aeson 2.1.0.0 -> 2.1.0.1 ✓
aeson 2.1.0.0 -> 2.1.1.0 ✗
```

### Major

Allows breaking changes:

```
aeson 2.1.0.0 -> 3.0.0.0 ✓
```

Also updates constraints in `.cabal`:

```cabal
# Before
build-depends: aeson ^>=2.1

# After --major
build-depends: aeson ^>=3.0
```

## What It Does

1. **Checks for updates** on Hackage
2. **Resolves compatible versions** with solver
3. **Updates lockfile** with new versions
4. **Optionally updates .cabal** constraints

## Package Index

Update the Hackage package index first:

```bash
# Update package index
cabal update

# Then update dependencies
hx update
```

## Selective Updates

Update only what you need:

```bash
# Update just aeson
hx update aeson

# Update text ecosystem
hx update text text-show text-builder

# Update all test dependencies
hx update hspec QuickCheck tasty
```

## After Updating

After updating dependencies:

```bash
# Build to verify
hx build

# Run tests
hx test

# Commit changes
git add hx.lock *.cabal
git commit -m "Update dependencies"
```

## Troubleshooting

### Solver Failures

If update fails due to conflicts:

```bash
# See what's conflicting
hx update --verbose

# Try updating one at a time
hx update aeson
hx update text
```

### Rollback

If an update breaks your build:

```bash
# Restore previous lockfile
git checkout hx.lock

# Sync dependencies
hx sync
```

## See Also

- [hx add](/docs/commands/add) — Add dependencies
- [hx outdated](/docs/commands/outdated) — Check for outdated deps
- [hx lock](/docs/commands/lock) — Generate lockfile
